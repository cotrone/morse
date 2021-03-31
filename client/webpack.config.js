const path = require('path')
const webpack = require('webpack')
const TerserPlugin = require('terser-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')

const comicData = require('./comic')

function buildComic(env, argv) {
  return {
    name: 'comic',
    entry: {
      comic: './src/index.ts',
    },
    output: {
      path: path.resolve(__dirname, 'dist'),
      filename: '[name].js',
    },
    module: {
      rules: [
        {
          test: /\.tsx?$/,
          use: 'ts-loader',
          exclude: /node_modules/,
        },
      ],
    },
    resolve: {
      extensions: ['.tsx', '.ts', '.js'],
    },
    optimization: {
      minimizer: [
        new TerserPlugin({
          extractComments: false,
        }),
      ],
    },
    plugins: [
      new webpack.BannerPlugin('code by chromako.de.'),
      new HtmlWebpackPlugin({
        inject: false,
        minify: false,
        scriptLoading: 'blocking',
        template: 'src/index.ejs',
        templateParameters: (compilation, assets, assetTags, options) => ({
          tags: assetTags,
          comic: comicData,
        }),
      }),
    ],
  }
}

module.exports = buildComic
