import type { BeepComicGlobal, MorseGlobal } from './index'

declare global {
  interface Window {
    BeepComic: BeepComicGlobal
    morse: MorseGlobal
  }
}
