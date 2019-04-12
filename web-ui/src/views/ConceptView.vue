<template>
  <div id="concept-view">
    <search-concept
      @select="switchConcept"
    ></search-concept>

    <v-progress-circular
      v-if="!concept && ui.loading"
      indeterminate
    ></v-progress-circular>

    <single-concept
      v-if="concept"
      v-model="concept"
    ></single-concept>
  </div>
</template>

<script>
import SearchConcept from '@/components/SearchConcept'
import SingleConcept from '@/components/SingleConcept'
import * as ConceptApi from '@/api/concept.js'

export default {
  name: 'ConceptView',
  components: {
    SearchConcept,
    SingleConcept
  },
  data () {
    return {
      concept: null,
      ui: {
        loading: false
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
      this.$router.push(`/concept/${uuid}`)
    }
  }
}
</script>
