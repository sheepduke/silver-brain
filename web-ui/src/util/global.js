import * as Event from '@/util/event'

export function alert(data) {
  Event.emit(Event.Type.ALERT, data)
}
