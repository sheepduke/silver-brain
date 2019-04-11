<template>
  <div id="home-view">
    <search-concept
      @select="selectConcept"
    ></search-concept>
  </div>
</template>

<script>
import SearchConcept from '@/components/SearchConcept'

export default {
  name: 'HomeView',
  components: {
    SearchConcept
  },
  data () {
    return {}
  },
  methods: {
    selectConcept (uuid) {
      this.$router.push(`/concept/${uuid}`)
    }
  }
}
</script>
