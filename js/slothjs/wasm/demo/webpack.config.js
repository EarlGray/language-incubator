const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  mode: 'development',
  entry: './src/index.js',
  experiments: {
    asyncWebAssembly: true,
    syncWebAssembly: true,
  },
  output: {
    filename: "main.js",
    path: path.resolve(__dirname, 'dist')
  },
  plugins: [
    new HtmlWebpackPlugin({title: "sljs demo"})
  ],
  devServer: {
    static: { directory: path.join(__dirname, 'dist')},
    compress: true,
  }
};
