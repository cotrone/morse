import comicData from '../comic'
import Comic from './Comic'

export class BeepComicGlobal {
  comicEl: HTMLDivElement = null

  draw(comicEl: HTMLDivElement) {
    comicEl.setAttribute(
      'style',
      `
        position: relative;
        display: flex;
        width: ${comicData.width}px;
        height: ${comicData.height}px;
        box-sizing: border-box;
        border: 2px solid black;
      `,
    )
    comicEl.innerHTML = `
      <label style="display: flex; width: 100%; height: 100%; align-items: center; justify-content: center">
        <input type="checkbox" style="outline: none">
      </label>
    `
    comicEl.title = comicData.alt
    this.comicEl = comicEl
  }

  send: (text: string) => void

  hurryUp: () => string
}

function main() {
  const { comicEl } = window.BeepComic

  const comic = new Comic(comicEl)
  comic.start()

  window.BeepComic.send = (morse: string) => {
    comic.send(morse)
  }

  window.BeepComic.hurryUp = () => {
    comic.impatient = true
    return "Ok fine. But you're like, totally breaking the immersion."
  }
}

document.addEventListener('DOMContentLoaded', main)

window.BeepComic = new BeepComicGlobal()
