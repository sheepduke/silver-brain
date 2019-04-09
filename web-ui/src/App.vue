<template>
  <div id="app">
    <v-container>
      <v-text-field
        label="Input concept name to start..."
        solo
        append-icon="search"
      ></v-text-field>
    </v-container>
  </div>
</template>

<script>
const axios = require('axios')

export default {
  name: 'App',
  data () {
    return {
      msg: 'Welcome to Your Vue.js App'
    }
  },
  beforeRouteEnter (from, to, next) {
    axios.get('/concepts').then(response => {
      console.log(response.data)
    })
  }
}
</script>

<style>

</style>
