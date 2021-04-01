import { apiEndpoint } from '../comic'

const PLAYBACK_DELAY = 250
const SEND_DELAY = 4000
const DOT_LENGTH = 150
const HUD_DELAY = 5000

function clickTimesToMorse(clickTimes: Array<number>) {
  return clickTimes
    .map((t) => {
      if (t < 0) {
        return Math.abs(t) > DOT_LENGTH * 3 ? ' ' : ''
      } else {
        return t > DOT_LENGTH * 3 ? '-' : '.'
      }
    })
    .join('')
    .replace(/^ /, '') // Strip a preceding space
}

class Client {
  stateId: string

  async say(morse: string): Promise<string> {
    const esc = (t: string) => t.replace(/ /g, '_')

    const url = [
      apiEndpoint,
      this.stateId ? esc(this.stateId) + '/' : '',
      esc(morse),
    ].join('')

    const resp = await fetch(url)
    if (!resp.ok) {
      throw new Error(`Unexpected response ${resp}`)
    }

    const data = await resp.text()

    const [state, respMorse] = data.split('/')
    this.stateId = state.trim()
    return respMorse.trim()
  }
}

class MorseHUD {
  HUD_LENGTH = 6
  HUD_CHAR_WIDTH = 50
  HUD_CHAR_SIZE = 7
  HUD_ANIM_DURATION = 200
  HUD_FAST_ANIM_DURATION = 75

  parentEl: HTMLDivElement
  el: HTMLDivElement
  shown: boolean
  lastMorse: string
  elStack: Array<HTMLDivElement>

  constructor(parentEl: HTMLDivElement) {
    this.parentEl = parentEl
    this.el = document.createElement('div')
    this.el.setAttribute(
      'style',
      `
        position: absolute;
        bottom: 50px;
        width: ${this.HUD_LENGTH * this.HUD_CHAR_WIDTH}px;
        height: ${this.HUD_CHAR_WIDTH}px;
        left: 50%;
        opacity: 0;
        transform: translateX(-50%);
        transition: all 5s ease-in;
        pointer-events: none;
      `,
    )
    this.parentEl.appendChild(this.el)
    this.lastMorse = ''
    this.elStack = []
    this.shown = false
  }

  update(morse: string) {
    if (!this.shown) {
      this.shown = true
      setTimeout(() => {
        this.el.style.opacity = '1'
      }, HUD_DELAY)
    }

    const lastMorse = this.lastMorse
    this.lastMorse = morse

    if (morse.length === 0) {
      const oldStack = this.elStack
      this.elStack = []
      for (const el of oldStack) {
        el.style.opacity = '0'
      }
      setTimeout(() => {
        for (const el of oldStack) {
          el.parentElement.removeChild(el)
        }
      }, this.HUD_ANIM_DURATION)
      return
    }

    if (morse.length > lastMorse.length) {
      const newBoxEl = document.createElement('div')
      newBoxEl.setAttribute(
        'style',
        `
          position: absolute;
          right: 0;
          display: flex;
          align-items: center;
          justify-content: center;
          width: ${this.HUD_CHAR_WIDTH}px;
          opacity: 0;
          transition: all ${this.HUD_ANIM_DURATION}ms ease-in-out;
          transform: translateY(7px);
        `,
      )
      const newCharEl = document.createElement('div')
      newCharEl.setAttribute(
        'style',
        `
          background: #999;
          transition: all ${this.HUD_FAST_ANIM_DURATION}ms ease-out;
          height: ${this.HUD_CHAR_SIZE}px;
        `,
      )
      newBoxEl.appendChild(newCharEl)
      this.el.appendChild(newBoxEl)
      this.el.offsetTop
      this.elStack.unshift(newBoxEl)
    }

    for (const [idx, boxEl] of this.elStack.entries()) {
      const char = morse[morse.length - 1 - idx]
      const charEl = boxEl.children[0] as HTMLDivElement
      if (char === '.') {
        charEl.style.width = `${this.HUD_CHAR_SIZE}px`
        charEl.style.borderRadius = `${this.HUD_CHAR_SIZE}px`
      } else if (char === '-') {
        charEl.style.width = `${Math.floor(3.5 * this.HUD_CHAR_SIZE)}px`
        charEl.style.borderRadius = '2px'
      } else {
        charEl.style.opacity = '0'
      }
      boxEl.style.opacity = '1'
      const x = -idx * this.HUD_CHAR_WIDTH
      boxEl.style.transform = `translateX(${Math.floor(x)}px)`
    }

    if (this.elStack.length > this.HUD_LENGTH) {
      const oldBoxEl = this.elStack.pop()
      oldBoxEl.style.opacity = '0'
      setTimeout(() => {
        this.el.removeChild(oldBoxEl)
      }, this.HUD_ANIM_DURATION)
    }
  }
}

export default class Comic {
  impatient: boolean
  el: HTMLDivElement
  hud: MorseHUD
  clickTimes: Array<number>
  updateInterval: number
  playTimeout: number
  sendTimeout: number
  lastOff: number
  lastOn: number
  client: Client

  constructor(el: HTMLDivElement) {
    this.el = el
    this.hud = new MorseHUD(el)
    this.sendTimeout = null
    this.updateInterval = null
    this.client = new Client()
    this.clickTimes = []
  }

  start() {
    const { el } = this
    const inputEl = el.querySelector('input')
    const labelEl = el.querySelector('label')

    let keyHeld = false

    this.lastOff = Date.now()
    this.lastOn = 0

    const setOn = (isOn: boolean) => {
      inputEl.checked = isOn
      if (isOn) {
        this.lastOn = Date.now()
      } else {
        this.lastOff = Date.now()
      }
      this.clickTimes.push(this.lastOff - this.lastOn)
      this.interpretClicks()
    }

    const handleDown = () => {
      clearTimeout(this.playTimeout)
      clearInterval(this.updateInterval)

      setOn(true)

      this.updateHUD()
      this.updateInterval = window.setInterval(() => {
        this.updateHUD()
      }, DOT_LENGTH)
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

  updateHUD() {
    // Display as if the user took a final action now.
    let clickTimes
    if (this.lastOn > this.lastOff) {
      clickTimes = [...this.clickTimes, Date.now() - this.lastOn + 1]
    } else {
      clickTimes = [...this.clickTimes, this.lastOff - Date.now()]
    }
    const morse = clickTimesToMorse(clickTimes)
    this.hud.update(morse)
  }

  interpretClicks() {
    const morse = clickTimesToMorse(this.clickTimes)

    clearTimeout(this.sendTimeout)
    this.sendTimeout = window.setTimeout(
      () => {
        this.send(morse)
      },
      this.impatient ? SEND_DELAY / 4 : SEND_DELAY,
    )
  }

  async send(morse: string) {
    const newClickTimes: Array<number> = []
    this.clickTimes = newClickTimes
    this.hud.update('')

    const text = window.morse.decode(morse)
    if (!text.length) {
      return
    }

    console.log(`Said: [${morse}] "${text}"`)

    const responseMorse = await this.client.say(morse)

    if (this.clickTimes != newClickTimes || this.clickTimes.length) {
      // If the user has input anything since, disregard this response.
      return
    }
    this.playback(responseMorse)
  }

  playback(morse: string) {
    const text = window.morse.decode(morse)

    if (this.impatient) {
      console.log(`Received: [${window.morse.encode(text)}] "${text}"`)
    }

    const inputEl = this.el.querySelector('input')

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
