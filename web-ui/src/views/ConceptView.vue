<template>
  <div id="concept-view">
    <v-layout>
      <v-flex md12>
        <search-concept @select="switchConcept"></search-concept>
      </v-flex>

      <v-flex>
        <v-btn round fab
               color="success"
               @click.stop="ui.showDialog = true">
          <v-icon dark large>add</v-icon>
        </v-btn>
      </v-flex>
    </v-layout>

    <v-progress-circular
      v-if="!concept && ui.loading"
      indeterminate
    ></v-progress-circular>

    <single-concept
      v-if="concept"
      v-model="concept"
    ></single-concept>

    <v-dialog max-width="50%" v-model="ui.showDialog">
      <new-concept @success="ui.showDialog = false" @close="ui.showDialog=false">
      </new-concept>
    </v-dialog>
  </div>
</template>

<script>
import SearchConcept from '@/components/SearchConcept'
import SingleConcept from '@/components/SingleConcept'
import NewConcept from '@/components/NewConcept'
import * as ConceptApi from '@/api/concept.js'

export default {
  name: 'ConceptView',
  components: {
    SearchConcept,
    SingleConcept,
    NewConcept
  },
  data () {
    return {
      concept: null,
      ui: {
        loading: false,
        showDialog: false
      }
    }
  },
  beforeRouteEnter (to, from, next) {
    next(vm => {
      let uuid = to.params.uuid
      vm.renderConcept(uuid)
    })
  },
  beforeRouteUpdate (to, from, next) {
    let uuid = to.params.uuid
    this.renderConcept(uuid)
    next()
  },
  methods: {
    async renderConcept (uuid) {
      this.concept = null
      this.ui.loading = true

      let [concept, parents, children, friends] = await Promise.all([
        ConceptApi.getConceptByUuid(uuid),
        ConceptApi.conceptParents(uuid),
        ConceptApi.conceptChildren(uuid),
        ConceptApi.conceptFriends(uuid)
      ])

      this.concept = await ({
        ...concept,
        parents: parents,
        children: children,
        friends: friends
      })

      this.ui.loading = false
    },
    async switchConcept (uuid) {
      this.$router.push(`/concepts/${uuid}`)
    }
  }
}
</script>
