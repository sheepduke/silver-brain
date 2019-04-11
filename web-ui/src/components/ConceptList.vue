<!--
     This component shows a list of concepts and their parent information.

     Props:
     * concepts: concepts to show.

     Events:
     * select: triggered when an item is selected.
-->

<template>
  <div id="concept-list">
    <v-list two-line>
      <v-list-tile
        v-for="concept in concepts"
        :key="concept.uuid"
        @click="select(concept)"
      >
        <v-list-tile-content>
          <v-list-tile-title>
            {{ concept.name }}
          </v-list-tile-title>

          <v-list-tile-sub-title>
            Parents: {{ parentsToString(concept.parents) }}
          </v-list-tile-sub-title>
        </v-list-tile-content>
      </v-list-tile>
    </v-list>
  </div>
</template>

<script>
export default {
  name: 'ConceptList',
  props: ['concepts'],
  data () {
    return {
    }
  },
  methods: {
    selectConcept (event) {
      console.log('selected')
    },
    parentsToString (parents) {
      return parents.map(parent => parent.name).join(',')
    },
    select (concept) {
      this.$emit('select', concept.uuid)
    }
  }
}
</script>
