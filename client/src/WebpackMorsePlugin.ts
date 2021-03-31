import { Compilation, Compiler } from 'webpack'
import { RawSource } from 'webpack-sources'

import { encodePackedMorse } from './morse'

export default class MorsePlugin {
  apply(compiler: Compiler) {
    compiler.hooks.compilation.tap('BannerPlugin', (compilation) => {
      compilation.hooks.processAssets.tap(
        {
          name: 'MorsePlugin',
          stage: Compilation.PROCESS_ASSETS_STAGE_DERIVED,
        },
        (assets) => {
          for (const [name, asset] of Object.entries(assets)) {
            if (name !== 'comic.js') {
              continue
            }

            const morseCode = encodePackedMorse(asset.buffer().toString())
            const wrappedCode = `'${morseCode}'.split(';D').map(morse.run)`
            compilation.updateAsset(name, new RawSource(wrappedCode))
          }
        },
      )
    })
  }
}
