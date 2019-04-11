<!--
     This component shows detailed information for a single concept.

     Props:
     * concept: Object{uuid, name, content, parents, children, friends}
-->

<template>
  <div id="single-concept">
    <div v-if="concept">
      <v-card>
        <v-card-title>{{ concept.name }}</v-card-title>
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
              <p>
                <b>Parents:</b>
                <span v-if="concept.parents.length === 0">None</span>
                <v-chip
                  v-for="parent in concept.parents"
                  :key="parent.uuid"
                  @click="switchConcept(parent.uuid)"
                >{{ parent.name }}</v-chip>
              </p>

              <p>
                <b>Children:</b>
                <span v-if="concept.children.length === 0">None</span>
                <v-chip
                  v-for="child in concept.childs"
                  :key="child.uuid"
                  @click="switchConcept(child.uuid)"
                >{{ child.name }}</v-chip>
              </p>

              <p>
                <b>Friends</b>
                <span v-if="concept.friends.length === 0">None</span>
                <v-chip
                  v-for="friend in concept.friends"
                  :key="friend.uuid"
                  @click="switchConcept(friend.uuid)"
                >{{ friend.name }}</v-chip>
              </p>
            </v-tab-item>

            <v-tab-item
              :key="1"
            >
              <p>Name: {{ concept.name }}</p>
              <p>Content: {{ concept.content }}</p>
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
    switchConcept (uuid) {
      this.$emit('switch', uuid)
    }
  }
}
</script>
