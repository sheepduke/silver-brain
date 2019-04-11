<template>
  <div id="home-view">
    <v-container>
      <v-layout>
        <v-flex md11>
          <search-concept
            @select="selectConcept"
          ></search-concept>
        </v-flex>
      </v-layout>
    </v-container>
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
