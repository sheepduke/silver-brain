<template>
  <v-app>
    <div id="app">
      <v-container>
        <v-layout>
          <alert-with-button
            v-model="alert.show"
            :message="alert.message"
            :color="alert.color"
            :button-text="alert.buttonText"
            :button-color="alert.buttonColor"
            @click="alert.buttonCallback"
            @close="alert.closeCallback"
          ></alert-with-button>

          <v-flex md1></v-flex>
          <v-flex md10>
            <router-view/>
          </v-flex>
        </v-layout>
      </v-container>
    </div>
  </v-app>
</template>

<script>
import AlertWithButton from '@/components/AlertWithButton'
import * as Event from '@/util/event'

export default {
  name: 'App',
  components: {
    AlertWithButton
  },
  data() {
    return {
      alert: this.initAlert()
    }
  },
  mounted() {
    Event.listen(Event.Type.ALERT, (data) => {
      this.alert = this.initAlert()
      this.alert = {
        ...this.alert,
        ...data,
        show: true
      }
    })
  },
  methods: {
    initAlert() {
      return {
        show: false,
        message: '',
        color: '',
        buttonText: '',
        buttonColor: '',
        buttonCallback: () => {},
        closeCallback: () => {
          this.alert.show = false
        }
      }
    }
  }
}
</script>

<style>
</style>
