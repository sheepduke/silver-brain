<template>
  <div id="home-view">
    <v-card>
      <v-card-text>
        <v-layout>

          <v-flex md12>
          <search-concept
            @select="selectConcept"
          ></search-concept>
          </v-flex>

          <v-btn fab>
            <v-icon
              large
              color="success"
            >
              add
            </v-icon>
          </v-btn>
        </v-layout>
      </v-card-text>

      <v-card-text>
        
      </v-card-text>
    </v-card>
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
