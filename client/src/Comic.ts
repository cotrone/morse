import { apiEndpoint } from '../comic'

const SEND_DELAY = 3000
const DOT_LENGTH = 150
const HUD_DELAY = 3000
const IDLE_DELAY = 30 * 1000

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

  escape(t: string) {
    return t.replace(/ /g, '_')
  }

  async say(morse: string): Promise<string> {
    const url = [
      apiEndpoint,
      '/.../',
      this.stateId ? this.escape(this.stateId) + '/' : '',
      this.escape(morse),
    ].join('')

    const resp = await fetch(url)
    if (!resp.ok) {
      throw new Error(`Unexpected response ${resp}`)
    }

    const data = await resp.text()

    const [state, ...respMorse] = data.split(' / ')
    this.stateId = state.trim()
    return respMorse.join(' / ').trim()
  }

  open(key: string) {
    const morseKey = window.morse.encode(key)
    window.open(`${apiEndpoint}/.-./${this.escape(morseKey)}`)
  }
}

class Speaker {
  TARGET_GAIN = 0.2

  isEnabled: boolean
  ctx: AudioContext
  gainNode: GainNode
  parentEl: HTMLElement
  el: HTMLButtonElement

  constructor(parentEl: HTMLElement) {
    this.isEnabled = false
    this.ctx = null
    this.gainNode = null

    this.parentEl = parentEl
    this.el = document.createElement('button')
    this.el.setAttribute(
      'style',
      `
        position: absolute;
        bottom: 6px;
        right: 6px;
        background: none;
        border: none;
        font-size: 20px;
        line-height: 20px;
        text-align: center;
        width: 30px;
        height: 30px;
        padding: 2px;
        box-sizing: content-box;
        opacity: 0;
        filter: grayscale(1);
        transition: all 5s linear;
      `,
    )
    this.el.addEventListener('click', () => {
      if (this.isEnabled) {
        this.disable()
      } else {
        this.enable()
      }
    })
    this.parentEl.appendChild(this.el)
    this.update()
  }

  show() {
    this.el.style.opacity = '.2'
  }

  update() {
    this.el.innerText = this.isEnabled ? '🔊' : '🔇'
    this.el.title = this.isEnabled ? 'Mute sound' : 'Enable sound'
  }

  enable() {
    this.isEnabled = true
    this.update()
  }

  disable() {
    this.isEnabled = false
    this.off()
    this.update()
  }

  on() {
    if (!this.isEnabled) {
      return
    }

    if (!this.ctx) {
      this.ctx = new AudioContext()

      const oscNode = this.ctx.createOscillator()
      this.gainNode = this.ctx.createGain()

      oscNode.type = 'sine'
      oscNode.frequency.value = 440
      oscNode.connect(this.gainNode)

      this.gainNode.gain.value = 0
      this.gainNode.connect(this.ctx.destination)

      oscNode.start()
    }

    this.gainNode.gain.cancelScheduledValues(this.ctx.currentTime)
    this.gainNode.gain.linearRampToValueAtTime(
      this.TARGET_GAIN,
      this.ctx.currentTime + 0.01,
    )
  }

  off() {
    if (!this.ctx) {
      return
    }
    this.gainNode.gain.linearRampToValueAtTime(
      0,
      this.ctx.currentTime + DOT_LENGTH / 1000 / 2,
    )
  }
}

class MorseHUD {
  HUD_LENGTH = 6
  HUD_CHAR_WIDTH = 50
  HUD_CHAR_SIZE = 7
  HUD_ANIM_DURATION = DOT_LENGTH
  HUD_FAST_ANIM_DURATION = DOT_LENGTH / 2

  parentEl: HTMLElement
  el: HTMLDivElement
  shown: boolean
  lastMorse: string
  elStack: Array<HTMLDivElement>

  constructor(parentEl: HTMLElement) {
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
        transition: all 5s linear;
        pointer-events: none;
      `,
    )
    this.parentEl.appendChild(this.el)
    this.lastMorse = ''
    this.elStack = []
    this.shown = false
  }

  show() {
    this.el.style.opacity = '1'
  }

  update(morse: string) {
    const lastMorse = this.lastMorse
    this.lastMorse = morse

    if (morse.length === 0) {
      const oldStack = this.elStack
      this.elStack = []
      for (const el of oldStack) {
        el.style.opacity = '0'
        el.style.transitionDuration = `${this.HUD_ANIM_DURATION * 2}ms`
      }
      setTimeout(() => {
        for (const el of oldStack) {
          el.parentElement.removeChild(el)
        }
      }, this.HUD_ANIM_DURATION)
      return
    }

    const newCount = morse.length - lastMorse.length
    for (let i = 0; i < newCount; i++) {
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
          transition: all ${this.HUD_ANIM_DURATION}ms ease-out;
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
      this.el.offsetTop // Force layout to set transition start values
      this.elStack.unshift(newBoxEl)
    }

    for (const [idx, boxEl] of this.elStack.entries()) {
      const char = morse[morse.length - 1 - idx]
      const charEl = boxEl.children[0] as HTMLDivElement
      if (char === '.') {
        charEl.style.width = `${this.HUD_CHAR_SIZE}px`
        charEl.style.borderRadius = `${this.HUD_CHAR_SIZE}px`
        charEl.style.opacity = '1'
      } else if (char === '-') {
        charEl.style.width = `${Math.floor(3.5 * this.HUD_CHAR_SIZE)}px`
        charEl.style.borderRadius = '2px'
        charEl.style.opacity = '1'
      } else {
        charEl.style.opacity = '0'
      }
      boxEl.style.opacity = '1'
      const x = -idx * this.HUD_CHAR_WIDTH
      boxEl.style.transform = `translateX(${Math.floor(x)}px)`
    }

    while (this.elStack.length > this.HUD_LENGTH) {
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
  speaker: Speaker
  isHUDShown: boolean
  hasInteracted: boolean
  clickTimes: Array<number>
  updateTimeout: number
  playTimeout: number
  sendTimeout: number
  playbackDelay: number
  lastOff: number
  lastOn: number
  client: Client

  constructor(el: HTMLDivElement) {
    this.el = el
    this.hud = new MorseHUD(el)
    this.speaker = new Speaker(this.el)
    this.isHUDShown = false
    this.hasInteracted = false
    this.sendTimeout = null
    this.updateTimeout = null
    this.client = new Client()
    this.clickTimes = []
    this.playbackDelay = 250
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
        this.speaker.on()
      } else {
        this.lastOff = Date.now()
        this.speaker.off()
      }
      this.clickTimes.push(this.lastOff - this.lastOn)
    }

    const handleDown = (ev: Event) => {
      ev.preventDefault()
      clearTimeout(this.playTimeout)
      clearTimeout(this.sendTimeout)

      setOn(true)

      this.hasInteracted = true
      this.showHUD()
      this.updateHUD()
    }

    const handleUp = (ev: Event) => {
      // Since this is hooked up to global event handlers, we must ensure the checkbox is actually being held.
      if (this.lastOn < this.lastOff) {
        return
      }

      ev.preventDefault()
      setOn(false)

      this.updateHUD()
      this.interpretClicks()
      inputEl.focus()
    }

    labelEl.addEventListener('mousedown', handleDown)
    labelEl.addEventListener('touchstart', handleDown)
    labelEl.addEventListener('keydown', (ev) => {
      if (!this.el.contains(ev.target as HTMLElement)) {
        return
      }

      if (!keyHeld && (ev.key === ' ' || ev.key === 'Enter')) {
        keyHeld = true
        handleDown(ev)
      }
    })

    window.addEventListener('mouseup', handleUp)
    window.addEventListener('touchend', handleUp)
    window.addEventListener('keyup', (ev) => {
      if (ev.key === ' ' || ev.key === 'Enter') {
        keyHeld = false
        handleUp(ev)
      }
    })

    labelEl.addEventListener('click', (ev: MouseEvent) => {
      ev.preventDefault()
    })

    this.printIntro()

    setTimeout(() => {
      if (!this.hasInteracted) {
        this.playback(window.morse.encode('CQ'))
      }
    }, IDLE_DELAY)
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

  showHUD() {
    if (!this.isHUDShown) {
      this.isHUDShown = true

      setTimeout(() => {
        this.hud.show()
      }, HUD_DELAY)

      setTimeout(() => {
        this.speaker.show()
      }, HUD_DELAY * 2)
    }
  }

  updateHUD() {
    // Display as if the user took a final action now.
    let clickTimes
    const now = Date.now()

    if (this.lastOn > this.lastOff) {
      clickTimes = [...this.clickTimes, now - this.lastOn + 1]
    } else {
      clickTimes = [...this.clickTimes, this.lastOff - now - 1]
    }

    let morse = clickTimesToMorse(clickTimes)
    if (this.lastOff > this.lastOn) {
      // Add a blank space for the next character if not holding down.
      morse += ' '
    }

    this.hud.update(morse)

    clearTimeout(this.updateTimeout)
    this.updateTimeout = window.setTimeout(() => {
      this.updateHUD()
    }, DOT_LENGTH)
  }

  interpretClicks() {
    const morse = clickTimesToMorse(this.clickTimes)

    clearTimeout(this.sendTimeout)
    this.sendTimeout = window.setTimeout(
      () => {
        this.send(morse)
      },
      this.impatient ? DOT_LENGTH * 7 : SEND_DELAY,
    )
  }

  async send(morse: string) {
    this.hasInteracted = true

    const newClickTimes: Array<number> = []
    this.clickTimes = newClickTimes
    this.hud.update('')

    const text = window.morse.decode(morse)

    console.log(`Said: [${morse}] "${text}"`)
    this.handleCommand(text)

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

    this.handleAction(text)

    const inputEl = this.el.querySelector('input')

    const delays: Array<[boolean, number]> = []
    for (const c of morse) {
      if (c === '.') {
        delays.push([true, this.playbackDelay])
        delays.push([false, this.playbackDelay])
      } else if (c === '-') {
        delays.push([true, this.playbackDelay * 3])
        delays.push([false, this.playbackDelay])
      } else if (c === ' ') {
        delays.push([false, this.playbackDelay * 3])
      }
    }
    delays.push([false, this.playbackDelay * 7])

    let idx = 0
    let hasPrinted = false
    const tick = () => {
      const [isOn, delay] = delays[idx]
      inputEl.checked = isOn

      if (isOn) {
        this.speaker.on()
      } else {
        this.speaker.off()
      }

      if (!this.impatient && idx === delays.length - 1 && !hasPrinted) {
        console.log(`Received: [${window.morse.encode(text)}] "${text}"`)
        hasPrinted = true
      }

      idx = (idx + 1) % delays.length
      clearTimeout(this.playTimeout)
      this.playTimeout = window.setTimeout(tick, delay)
    }

    tick()
  }

  handleCommand(text: string) {
    if (text === 'BEEP') {
      this.speaker.enable()
    } else if (text === 'MUTE' || text === 'QUIET') {
      this.speaker.disable()
    } else if (text === 'QRS') {
      this.playbackDelay = Math.min(500, this.playbackDelay * 1.2)
    } else if (text === 'QRQ') {
      this.playbackDelay = Math.max(50, this.playbackDelay * (1 / 1.2))
    }
  }

  handleAction(text: string) {
    if (text.startsWith('//')) {
      this.client.open(text.substr(2))
    }
  }
}
