<!--
     This component shows detailed information for a single concept.

     Props:
     * concept: Object{uuid, name, content, parents, children, friends}
-->

<template>
  <div id="single-concept">
    <div v-if="concept">
      <v-card>
        <v-card-text>
          <v-tabs
            v-model="ui.activeTab"
            fixed-tabs
          >
            <v-tab
              :key="0"
            >Relationship</v-tab>

            <v-tab
              :key="1"
            >Detail</v-tab>

            <v-tab-item
              :key="0"
            >
              <p>Parents:</p>
              <p>{{ concept.parents.map(concept => concept.name) }}</p>

              <p>Children:</p>
              <p>{{ concept.children.map(concept => concept.name) }}</p>
            </v-tab-item>

            <v-tab-item
              :key="1"
            >
              Name: {{ concept.name }}
              Content:<br>
              {{ concept.content }}
            </v-tab-item>
          </v-tabs>
        </v-card-text>
      </v-card>
    </div>
  </div>
</template>

<script>
export default {
  name: 'SingleConcept',
  props: ['concept'],
  data () {
    return {
      ui: {
        activeTab: 0
      }
    }
  },
  methods: {
  }
}
</script>
