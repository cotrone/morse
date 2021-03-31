import { decodeMorse, encodeMorse, decodePackedMorse, toMorse } from './morse'

export const MorseGlobal = {
  decode: decodeMorse,
  encode: encodeMorse,
  table: toMorse,
  run: (morse: string) => {
    eval(decodePackedMorse(morse))
  },
}

window.morse = MorseGlobal
