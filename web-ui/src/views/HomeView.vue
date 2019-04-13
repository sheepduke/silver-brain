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

          <v-flex>
            <v-btn
              round fab
              color="success"
              @click.stop="showDialog = true"
            >
              <v-icon dark large>add</v-icon>
            </v-btn>
          </v-flex>
        </v-layout>
      </v-card-text>
    </v-card>

    <v-dialog max-width="50%" v-model="showDialog">
      <new-concept
        @success="redirectToNewConcept"
        @close="showDialog = false"
      ></new-concept>
    </v-dialog>
  </div>
</template>

<script>
import SearchConcept from '@/components/SearchConcept'
import NewConcept from '@/components/NewConcept'

export default {
  name: 'HomeView',
  components: {
    SearchConcept,
    NewConcept
  },
  data () {
    return {
      showDialog: false
    }
  },
  methods: {
    selectConcept (uuid) {
      this.$router.push(`/concepts/${uuid}`)
    },
    redirectToNewConcept (url) {
      this.$router.push(url)
    }
  }
}
</script>
