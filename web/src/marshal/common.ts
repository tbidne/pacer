type Title = {
  display: boolean;
  text: string;
};

type YAxesT<A> = {
  y: A;
  y1?: A;
};

function mapYAxes<A, B>(f: (_: A) => B, a: YAxesT<A>): YAxesT<B> {
  const yAxesB: YAxesT<B> = {
    y: f(a.y),
  };
  const y1 = a.y1;
  if (y1 != null) {
    yAxesB.y1 = f(y1);
  }
  return yAxesB;
}

type YAxisId = "y" | "y1";

type YAxisLabel = DistanceUnit | "pace /km" | "pace /mi" | "time";

type DistanceUnit = "m" | "km" | "mi";

type YAxisType = "distance" | "duration" | "pace";

export {
  DistanceUnit,
  Title,
  mapYAxes,
  YAxesT,
  YAxisId,
  YAxisLabel,
  YAxisType,
};
