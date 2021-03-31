import type { BeepComicGlobal } from './index'
import type { MorseGlobal } from './morseIndex'

declare global {
  interface Window {
    BeepComic: BeepComicGlobal
    morse: typeof MorseGlobal
  }
}
