<!--
     This component shows detailed information for a single concept.

     Props:
     * concept: Object{uuid, name, content, parents, children, friends}
-->

<template>
  <div id="single-concept">
    <div v-if="concept">
      <v-card>
        <v-card-title><h2>{{ concept.name }}</h2></v-card-title>

        <v-divider></v-divider>

        <v-card-text>
          <p>
            <b>Parents:</b>
            <span v-if="concept.parents.length === 0">None</span>
            <concept-tag
              v-for="(parent, index) in concept.parents"
              :key="parent.uuid"
              v-model="concept.parents[index]"
              close
              @close="removeConceptRelation(concept.parents, index)"
            ></concept-tag>
          </p>

          <p>
            <b>Children:</b>
            <span v-if="concept.children.length === 0">None</span>

            <concept-tag
              v-for="(child, index) in concept.children"
              :key="child.uuid"
              v-model="concept.children[index]"
              close
              @close="removeConceptRelation(concept.children, index)"
            ></concept-tag>
          </p>

          <p>
            <b>Friends</b>
            <span v-if="concept.friends.length === 0">None</span>
            <concept-tag
              v-for="(friend, index) in concept.friends"
              :key="friend.uuid"
              v-model="concept.friends[index]"
              close
              @close="removeConceptRelation(concept.friends, index)"
            ></concept-tag>
          </p>
        </v-card-text>

        <v-divider></v-divider>

        <v-card-text>
          {{ concept.content }}
        </v-card-text>

        <alert-with-button
          v-model="alert.show"
          :message="alert.message"
          :button-text="alert.buttonText"
          button-color="primary"
          @click="undoRemoveConceptRelation"
          @close="alert.show = false"
        ></alert-with-button>
      </v-card>
    </div>
  </div>
</template>

<script>
import AlertWithButton from '@/components/AlertWithButton'
import ConceptTag from '@/components/ConceptTag'

export default {
  name: 'SingleConcept',
  components: {
    AlertWithButton,
    ConceptTag
  },
  props: {
    concept: {
      type: Object,
      default: null
    }
  },
  data () {
    return {
      alert: {
        show: false,
        message: '',
        buttonText: ''
      },
      removed: {
        uuid: '',
        name: '',
        type: ''
      }
    }
  },
  methods: {
    async removeConceptRelation (collection, index) {
      let removedConcept = collection[index]
      this.removed.uuid = removedConcept.uuid
      this.removed.name = removedConcept.name

      switch (collection) {
        case this.concept.parents:
          this.removed.type = 'parents'
          break
        case this.concept.children:
          this.removed.type = 'children'
          break
        case this.concept.friends:
          this.removed.type = 'friends'
          break
      }

      // TODO call API

      this.alert.message = `"${removedConcept.name}" removed from ${this.removed.type}`
      this.alert.buttonText = 'Undo'
      this.alert.show = true
    },
    async undoRemoveConceptRelation () {
      // TODO call API

      this.alert.message = 'Removal undone'
      this.alert.buttonText = ''
      this.alert.show = true
    }
  }
}
</script>
