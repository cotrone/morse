import comicData from '../comic'
import Comic from './Comic'
import { decodeMorse, encodeMorse } from './morse'

export class BeepComicGlobal {
  comicEl: HTMLDivElement = null

  draw(comicEl: HTMLDivElement) {
    comicEl.setAttribute(
      'style',
      `
        display: flex;
        width: ${comicData.width}px;
        height: ${comicData.height}px;
        box-sizing: border-box;
        border: 2px solid black;
      `,
    )
    comicEl.innerHTML = `
      <label style="display: flex; width: 100%; height: 100%; align-items: center; justify-content: center">
        <input type="checkbox">
      </label>
    `
    comicEl.title = comicData.alt
    this.comicEl = comicEl
  }
}

export const MorseGlobal = {
  decode: decodeMorse,
  encode: encodeMorse,
}

function main() {
  const { comicEl } = window.BeepComic
  const comic = new Comic(comicEl)
  comic.start()
}

document.addEventListener('DOMContentLoaded', main)

window.BeepComic = new BeepComicGlobal()
window.morse = MorseGlobal
