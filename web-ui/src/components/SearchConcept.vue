<!--
     This component displays a single search box and its results.

     Properties: none.

     Events:
     * selected: triggered when a search result is selected.
-->

<template>
  <div id="search">
    <v-card flat color="#eae7e1">
            <v-card-text>
              <v-text-field
                v-model="search"
                label="Input concept name to start..."
                solo
                append-icon="search"
                @input="searchConcept"
              ></v-text-field>
            </v-card-text>

            <!-- <v-dialog max-width="50%" v-model="newConcept.showDialog">
                 <template v-slot:activator="{ on }">
                 <v-card-text class="float-btn-wrapper">
                 <v-btn absolute top right round
                 :loading="newConcept.buttonLoading"
                 :disabled="newConcept.buttonLoading"
                 v-on="on"
                 color="success">
                 New Concept
                 <v-icon right dark>add</v-icon>
                 </v-btn>
                 </v-card-text>
                 </template>

                 <new-concept
                 @close="newConcept.showDialog = false"
                 @submit="createNewConcept"
                 ></new-concept>
                 </v-dialog> -->

            <v-card-text ref="searchResultBox" hidden>
              <concept-list
                v-bind:concepts="concepts"
                @select="selectConcept"
              ></concept-list>
            </v-card-text>

          </v-card>
  </div>
</template>

<script>
import * as ConceptApi from '@/api/concept.js'
import NewConcept from '@/components/NewConcept'
import ConceptList from '@/components/ConceptList'

export default {
  name: 'SearchConcept',
  components: {
    NewConcept,
    ConceptList
  },
  data () {
    return {
      search: '',
      concepts: [],
      newConcept: {
        buttonLoading: false,
        showDialog: false
      }
    }
  },
  methods: {
    reset () {
      this.search = ''
      this.concepts = []
      this.newConcept = {
        buttonLoading: false,
        showDialog: false
      }
      this.setSearchResultBoxHidden(true)
    },
    async createNewConcept (name, content, contentFormat) {
      this.newConcept.showDialog = false
      this.newConcept.buttonLoading = true
      try {
        await ConceptApi.newConcept(name, content, contentFormat)
      } catch (err) {
        console.error(err)
      }
      this.newConcept.buttonLoading = false
    },
    async searchConcept () {
      this.setSearchResultBoxHidden(false)
      let concepts = await ConceptApi.searchConcept(this.search)
      for (let index in concepts) {
        let concept = concepts[index]
        concept.parents = await ConceptApi.conceptParents(concept.uuid)
      }
      this.concepts = concepts
    },
    async selectConcept (uuid) {
      this.reset()
      this.$emit('select', uuid)
    },
    setSearchResultBoxHidden (statusFlag) {
      this.$refs.searchResultBox.hidden = statusFlag
    }
  }
}
</script>

<style>
.float-btn-wrapper {
  position: relative;
  height: 50px;
}
</style>
