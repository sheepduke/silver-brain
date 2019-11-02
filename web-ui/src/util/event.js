import Vue from 'vue'

export const Bus = new Vue()

export const Type = Object.freeze({
  ALERT: 'alert'
})

export function emit(event, data) {
  Bus.$emit(event, data)
}

export function listen(event, callback) {
  Bus.$on(event, callback)
}
