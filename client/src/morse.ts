import BaseX from '@multiformats/base-x'

// via https://github.com/ozdemirburak/morse-decoder/blob/0d750bac6c4ab7000c32fcc7c662b87e2faa55bd/src/index.js
export const toMorse = new Map([
  ['0', '-----'],
  ['1', '.----'],
  ['2', '..---'],
  ['3', '...--'],
  ['4', '....-'],
  ['5', '.....'],
  ['6', '-....'],
  ['7', '--...'],
  ['8', '---..'],
  ['9', '----.'],
  ['A', '.-'],
  ['B', '-...'],
  ['C', '-.-.'],
  ['D', '-..'],
  ['E', '.'],
  ['F', '..-.'],
  ['G', '--.'],
  ['H', '....'],
  ['I', '..'],
  ['J', '.---'],
  ['K', '-.-'],
  ['L', '.-..'],
  ['M', '--'],
  ['N', '-.'],
  ['O', '---'],
  ['P', '.--.'],
  ['Q', '--.-'],
  ['R', '.-.'],
  ['S', '...'],
  ['T', '-'],
  ['U', '..-'],
  ['V', '...-'],
  ['W', '.--'],
  ['X', '-..-'],
  ['Y', '-.--'],
  ['Z', '--..'],
  ['.', '.-.-.-'],
  [',', '--..--'],
  ['?', '..--..'],
  ["'", '.----.'],
  ['!', '-.-.--'],
  ['/', '-..-.'],
  ['(', '-.--.'],
  [')', '-.--.-'],
  ['&', '.-...'],
  [':', '---...'],
  [';', '-.-.-.'],
  ['=', '-...-'],
  ['+', '.-.-.'],
  ['-', '-....-'],
  ['_', '..--.-'],
  ['"', '.-..-.'],
  ['$', '...-..-'],
  ['@', '.--.-.'],
])

export const fromMorse = new Map(
  Array.from(toMorse.entries(), ([k, v]) => [v, k]),
)

export function decodeMorse(morse: string) {
  return morse
    .split(' ')
    .map((c) => (c === '/' ? ' ' : fromMorse.get(c) || ''))
    .join('')
}

export function encodeMorse(text: string) {
  return text
    .split('')
    .map((c) => (c === ' ' ? '/' : toMorse.get(c.toUpperCase()) || null))
    .filter((x) => x)
    .join(' ')
}

export const packedMorseEncoder = BaseX([...toMorse.keys()].join(''))

export function encodePackedMorse(data: string) {
  const textEncoder = new TextEncoder()
  const packed = packedMorseEncoder.encode(textEncoder.encode(data))
  return encodeMorse(packed)
}

export function decodePackedMorse(morse: string) {
  const data = packedMorseEncoder.decode(decodeMorse(morse))
  const textDecoder = new TextDecoder()
  return textDecoder.decode(data)
}
