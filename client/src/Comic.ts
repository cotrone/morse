import { apiEndpoint } from '../comic'
import { decodeMorse, encodeMorse, toMorse } from './morse'

const HOLD_DELAY = 150
const PLAYBACK_DELAY = 250
const SEND_DELAY = 4000

function clickTimesToMorse(clickTimes: Array<number>) {
  const meanHold =
    clickTimes.filter((x) => x > 0).reduce((a, b) => a + b, 0) /
    clickTimes.length
  const thresh = Math.max(200, meanHold)
  return clickTimes
    .map((t) => {
      if (t < 0) {
        return Math.abs(t) > thresh * 4 ? ' ' : ''
      } else {
        return t > thresh * 2 ? '-' : '.'
      }
    })
    .join('')
}

type ServerSayResponse = {
  session: string
  say: string
}

class Client {
  sessionId: string

  async say(text: string): Promise<string> {
    const resp = await fetch(
      `${apiEndpoint}${this.sessionId ? this.sessionId + '/' : ''}${text}`,
    )
    const data: ServerSayResponse = await resp.json()
    this.sessionId = data.session
    return data.say
  }
}

export default class Comic {
  consoleEnabled: boolean
  el: HTMLDivElement
  clickTimes: Array<number>
  playbackText: string
  playTimeout: number
  sendTimeout: number
  client: Client

  constructor(el: HTMLDivElement) {
    this.el = el
    this.sendTimeout = null
    this.client = new Client()
    this.clickTimes = []
    this.consoleEnabled = false
  }

  start() {
    const { el } = this
    const inputEl = el.querySelector('input')
    const labelEl = el.querySelector('label')

    let lastOff: number = Date.now()
    let lastOn: number = 0
    let holdTimeout: number = null

    const setOn = (isOn: boolean) => {
      inputEl.checked = isOn
      if (isOn) {
        lastOn = Date.now()
      } else {
        lastOff = Date.now()
      }
      this.clickTimes.push(lastOff - lastOn)
      this.interpretClicks()
    }

    const handleDown = (ev: MouseEvent) => {
      clearTimeout(this.playTimeout)
      clearTimeout(holdTimeout)
      holdTimeout = window.setTimeout(() => {
        setOn(true)
        holdTimeout = null
      }, HOLD_DELAY)
    }

    const handleUp = (ev: MouseEvent) => {
      if (holdTimeout) {
        setOn(!inputEl.checked)
      } else {
        setOn(false)
      }

      clearTimeout(holdTimeout)
      holdTimeout = null
    }

    labelEl.addEventListener('mousedown', handleDown)
    labelEl.addEventListener('touchstart', handleDown)

    labelEl.addEventListener('mouseup', handleUp)
    labelEl.addEventListener('touchend', handleUp)

    labelEl.addEventListener('click', (ev: MouseEvent) => {
      ev.preventDefault()
    })

    this.printIntro()
  }

  printIntro() {
    const lines = []
    lines.push(' MORSE CODE ')
    lines.push('------------')
    for (const [char, code] of toMorse) {
      lines.push(` ${char} : ${code}`)
    }
    console.log(lines.join('\n'))
  }

  interpretClicks() {
    const morse = clickTimesToMorse(this.clickTimes).replace(/^ /, '') // Strip a preceding space
    const text = decodeMorse(morse)

    clearTimeout(this.sendTimeout)
    this.sendTimeout = window.setTimeout(async () => {
      console.log(`Said: [${morse}] "${text}"`)

      const newClickTimes: Array<number> = []
      this.clickTimes = newClickTimes

      if (!text.length) {
        return
      }

      const responseText = await this.client.say(text)

      if (this.clickTimes != newClickTimes || this.clickTimes.length) {
        // If the user has input anything since, disregard this response.
        return
      }
      this.playback(responseText)
    }, SEND_DELAY)
  }

  playback(text: string) {
    const inputEl = this.el.querySelector('input')
    this.playbackText = text
    const morse = encodeMorse(text)

    const delays: Array<[boolean, number]> = []
    for (const c of morse) {
      if (c === '.') {
        delays.push([true, PLAYBACK_DELAY])
        delays.push([false, PLAYBACK_DELAY])
      } else if (c === '-') {
        delays.push([true, PLAYBACK_DELAY * 3])
        delays.push([false, PLAYBACK_DELAY])
      } else if (c === ' ') {
        delays.push([false, PLAYBACK_DELAY * 3])
      }
    }
    delays.push([false, PLAYBACK_DELAY * 7])

    let idx = 0
    let hasPrinted = false
    const tick = () => {
      const [isOn, delay] = delays[idx]
      inputEl.checked = isOn

      if (idx === delays.length - 1 && !hasPrinted) {
        console.log(`Received: [${encodeMorse(text)}] "${text}"`)
        hasPrinted = true
      }

      idx = (idx + 1) % delays.length
      this.playTimeout = window.setTimeout(tick, delay)
    }

    tick()
  }
}
