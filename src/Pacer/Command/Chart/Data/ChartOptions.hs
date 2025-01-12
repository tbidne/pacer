module Pacer.Command.Chart.Data.ChartOptions
  ( ChartOptions (..),
    mkChartOptions,
  )
where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON))
import Data.Aeson qualified as Asn
import Data.Aeson.Types (Pair)
import Pacer.Command.Chart.Data.ChartRequest
  ( ChartRequest (title, y1Axis, yAxis),
    YAxisType
      ( YAxisDistance,
        YAxisDuration,
        YAxisPace
      ),
  )
import Pacer.Data.Distance (DistanceUnit)
import Pacer.Prelude

-- | Possible y-axis positions.
data YPosition
  = YLeft
  | YRight
  deriving stock (Eq, Show)

instance ToJSON YPosition where
  toJSON YLeft = "left"
  toJSON YRight = "right"

-- | Y-axis options.
data YOptions = MkYOptions
  { -- | Label that appears on the (vertical) y-axis itself.
    label :: Text,
    -- | Y-axis position.
    position :: YPosition
  }
  deriving stock (Eq, Show)

instance ToJSON YOptions where
  toJSON y =
    Asn.object
      $ [ "position" .= y.position,
          "title"
            .= Asn.object
              [ "color" .= textColor,
                "display" .= True,
                labelFontOpts,
                "text" .= y.label
              ]
        ]
      ++ commonOpts

-- | Chart option data.
data ChartOptions = MkChartOptions
  { -- | Overall chart title.
    title :: Text,
    -- | Y-axis options.
    yOptions :: YOptions,
    -- | Possible y1-axis options.
    y1Options :: Maybe YOptions
  }
  deriving stock (Eq, Show)

instance ToJSON ChartOptions where
  toJSON c =
    Asn.object
      [ "plugins"
          .= Asn.object
            [ "legend" .= legendOpts,
              "title"
                .= Asn.object
                  [ "align" .= t "center",
                    "color" .= textColor,
                    "display" .= True,
                    fontOpts 24,
                    "text" .= c.title
                  ],
              "tooltip" .= tooltipOpts
            ],
        "pointHitRadius" .= i 20, -- tooltip hitbox
        "responsive" .= True,
        "maintainAspectRatio" .= False,
        "scales" .= scales
      ]
    where
      scales =
        Asn.object
          $ [ "x"
                .= Asn.object
                  ( [ "time"
                        .= Asn.object
                          [ "displayFormats"
                              .= Asn.object
                                [ -- NOTE: Keeping the lablel relatively concise as it
                                  -- is more readable, and the tooltip contains the full
                                  -- timestamp anyway.
                                  "day" .= t "dd MMM yy"
                                ],
                            "unit" .= t "day"
                          ],
                      "title"
                        .= Asn.object
                          [ "color" .= textColor,
                            "display" .= True,
                            labelFontOpts,
                            "text" .= t "datetime"
                          ],
                      -- NOTE: timeseries over cartesian (string "time") as the
                      -- spaces out events equally, while the latter spaces
                      -- relative to the actual time difference. But this is
                      -- usually quite silly as e.g. official marathons may be
                      -- very spaces out, and this provides no actual value.
                      "type" .= t "timeseries"
                    ]
                      ++ commonOpts
                  ),
              "y" .= c.yOptions
            ]
          ++ maybe [] (\z -> ["y1" .= z]) c.y1Options

      legendOpts =
        Asn.object
          [ "labels"
              .= Asn.object
                ["color" .= textColor]
          ]

      tooltipOpts =
        Asn.object
          [ "backgroundColor" .= tooltipBackgroundColor,
            "bodyColor" .= tooltipTextColor,
            "titleColor" .= tooltipTextColor
          ]

-- | Creates a chart options from the request. The distance unit is used
-- for labeling the axis.
mkChartOptions :: DistanceUnit -> ChartRequest a -> ChartOptions
mkChartOptions dunit request =
  MkChartOptions
    { title = request.title,
      yOptions,
      y1Options
    }
  where
    (yOptions, y1Options) = case request.y1Axis of
      Nothing ->
        let lbl = mkYLabel request.yAxis
            opts = MkYOptions lbl YLeft
         in (opts, Nothing)
      Just y1Axis ->
        let lbl = mkYLabel request.yAxis
            opts = MkYOptions lbl YLeft
            lbl1 = mkYLabel y1Axis
            opts1 = MkYOptions lbl1 YRight
         in (opts, Just opts1)

    mkYLabel :: YAxisType -> Text
    mkYLabel = \case
      YAxisDistance -> dstTxt
      YAxisDuration -> "time"
      YAxisPace -> "pace /" <> dstTxt
      where
        dstTxt = display dunit

commonOpts :: List Pair
commonOpts =
  [ gripOpts,
    ticksOpts
  ]
  where
    gripOpts =
      "grid"
        .= Asn.object
          [ "color" .= gridColor
          ]
    ticksOpts =
      "ticks"
        .= Asn.object
          [ "color" .= textColor
          ]

labelFontOpts :: Pair
labelFontOpts = fontOpts 16

fontOpts :: Int -> Pair
fontOpts sz =
  "font"
    .= Asn.object
      [ "size" .= i sz
      ]

gridColor :: Text
gridColor = "#393939"

-- NOTE: [Text Color]
textColor :: Text
textColor = "#c3c3c3"

tooltipBackgroundColor :: Text
tooltipBackgroundColor = "rgba(204, 204, 204, 0.75)"

tooltipTextColor :: Text
tooltipTextColor = "black"

-- | TODO: This can be replaced with -XNamedDefaults once it is available.
t :: Text -> Text
t = id

i :: Int -> Int
i = id
