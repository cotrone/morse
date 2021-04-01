import { apiEndpoint } from '../comic'

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
  state: string
  say: string
}

class Client {
  stateId: string

  async say(text: string): Promise<string> {
    const resp = await fetch(
      `${apiEndpoint}${this.stateId ? this.stateId + '/' : ''}${text}`,
    )
    const data: ServerSayResponse = await resp.json()
    this.stateId = data.state
    return data.say
  }
}

export default class Comic {
  impatient: boolean
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
  }

  start() {
    const { el } = this
    const inputEl = el.querySelector('input')
    const labelEl = el.querySelector('label')

    let lastOff: number = Date.now()
    let lastOn: number = 0
    let keyHeld = false

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

    const handleDown = () => {
      clearTimeout(this.playTimeout)
      setOn(true)
    }

    const handleUp = () => {
      setOn(false)
      inputEl.focus()
    }

    labelEl.addEventListener('mousedown', handleDown)
    labelEl.addEventListener('touchstart', handleDown)
    labelEl.addEventListener('keydown', (ev) => {
      if (!keyHeld && ev.key === ' ') {
        keyHeld = true
        handleDown()
      }
    })

    labelEl.addEventListener('mouseup', handleUp)
    labelEl.addEventListener('touchend', handleUp)
    labelEl.addEventListener('keyup', (ev) => {
      if (ev.key === ' ') {
        keyHeld = false
        handleUp()
      }
    })

    labelEl.addEventListener('click', (ev: MouseEvent) => {
      ev.preventDefault()
    })

    this.printIntro()
  }

  printIntro() {
    const lines = []
    lines.push(' MORSE CODE ')
    lines.push('------------')
    for (const [char, code] of window.morse.table) {
      lines.push(` ${char} [${code}]`)
    }
    console.log(lines.join('\n'))
  }

  interpretClicks() {
    const morse = clickTimesToMorse(this.clickTimes).replace(/^ /, '') // Strip a preceding space

    clearTimeout(this.sendTimeout)
    this.sendTimeout = window.setTimeout(
      () => {
        this.send(morse)
      },
      this.impatient ? SEND_DELAY / 4 : SEND_DELAY,
    )
  }

  async send(morse: string) {
    const text = window.morse.decode(morse)

    const newClickTimes: Array<number> = []
    this.clickTimes = newClickTimes

    if (!text.length) {
      return
    }

    console.log(`Said: [${morse}] "${text}"`)

    const responseText = await this.client.say(text)

    if (this.clickTimes != newClickTimes || this.clickTimes.length) {
      // If the user has input anything since, disregard this response.
      return
    }
    this.playback(responseText)
  }

  playback(text: string) {
    if (this.impatient) {
      console.log(`Received: [${window.morse.encode(text)}] "${text}"`)
    }

    const inputEl = this.el.querySelector('input')
    this.playbackText = text
    const morse = window.morse.encode(text)

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

      if (!this.impatient && idx === delays.length - 1 && !hasPrinted) {
        console.log(`Received: [${window.morse.encode(text)}] "${text}"`)
        hasPrinted = true
      }

      idx = (idx + 1) % delays.length
      this.playTimeout = window.setTimeout(tick, delay)
    }

    tick()
  }
}
