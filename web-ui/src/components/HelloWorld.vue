<template>
  <div class="hello">
    This is not true
  </div>
</template>

<script>
const axios = require('axios')

export default {
  name: 'HelloWorld',
  data () {
    return {
      msg: 'Welcome to Your Vue.js App'
    }
  },
  beforeRouteEnter (from, to, next) {
    axios.get('/concepts/').then(response => {
      console.log(response.data)
    })
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
h1, h2 {
  font-weight: normal;
}
ul {
  list-style-type: none;
  padding: 0;
}
li {
  display: inline-block;
  margin: 0 10px;
}
a {
  color: #42b983;
}
</style>
