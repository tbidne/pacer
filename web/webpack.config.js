const HtmlWebpackPlugin = require('html-webpack-plugin');

//const path = require('path');

/*{
  resolve: {
    fallback: {
      "http": require.resolve("stream-http"),
      "https": require.resolve("https-browserify")
    }
  }
}*/

module.exports = {
  mode: 'development',
  entry: './src/index.js',
  resolve: {
    fallback: {
      "buffer": require.resolve("buffer/"),
      "http": require.resolve("stream-http"),
      "https": require.resolve("https-browserify"),
      "url": require.resolve("url/"),
    }
  },
  plugins: [new HtmlWebpackPlugin({
    template: './src/index.html',
    header: '<h1>A title</h1>',
    anyElement: '<b>I am an html element</b>'
  })],
};
