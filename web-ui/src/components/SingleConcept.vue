<!--
     This component shows detailed information for a single concept.

     Props:
     * value: Object{uuid, name, content, parents, children, friends}
-->

<template>
  <div id="single-concept">
    <div v-if="value">
      <v-card>
        <v-card-title><h2>{{ value.name }}</h2></v-card-title>

        <v-divider></v-divider>

        <v-card-text>
          <p>
            <b>Parents:</b>
            <concept-tag
              v-for="(parent, index) in value.parents"
              :key="parent.uuid"
              v-model="value.parents[index]"
              close
              @close="removeConceptRelation(Enum.Relation.PARENT, index)"
            ></concept-tag>
            <v-btn
              small round
              color="success"
              @click="addConceptRelation(Enum.Relation.PARENT)"
            >
              Add<v-icon right>add</v-icon>
            </v-btn>
          </p>

          <p>
            <b>Children:</b>
            <concept-tag
              v-for="(child, index) in value.children"
              :key="child.uuid"
              v-model="value.children[index]"
              close
              @close="removeConceptRelation(Enum.Relation.CHILD, index)"
            ></concept-tag>
            <v-btn
              small round
              color="success"
              @click="addConceptRelation(Enum.Relation.CHILD)"
            >
              Add<v-icon right>add</v-icon>
            </v-btn>
          </p>

          <p>
            <b>Friends</b>
            <concept-tag
              v-for="(friend, index) in value.friends"
              :key="friend.uuid"
              v-model="value.friends[index]"
              close
              @close="removeConceptRelation(Enum.Relation.FRIEND, index)"
            ></concept-tag>
            <v-btn
              small round
              color="success"
              @click="addConceptRelation(Enum.Relation.FRIEND)"
            >
              Add<v-icon right>add</v-icon>
            </v-btn>
          </p>
        </v-card-text>

        <v-divider></v-divider>

        <v-card-text>
          {{ value.content }}
        </v-card-text>

        <!-- <alert-with-button
             v-model="alert.show"
             :message="alert.message"
             :color="alert.color"
             :button-text="alert.buttonText"
             button-color="primary"
             @click="undoRemoveConceptRelation"
             @close="alert.show = false"
             ></alert-with-button> -->
      </v-card>
    </div>
  </div>
</template>

<script>
import ConceptTag from '@/components/ConceptTag'
import * as ConceptApi from '@/api/concept'
import * as Enum from '@/util/enum'
import * as Global from '@/util/global'

export default {
  name: 'SingleConcept',
  components: {
    ConceptTag
  },
  props: {
    value: {
      type: Object,
      default: null
    }
  },
  data () {
    return {
      removed: {
        concept: null,
        type: null,
        index: null
      },
      Enum: Enum
    }
  },
  methods: {
    async removeConceptRelation (type, index) {
      let collection = this.value[type]
      let removedConcept = collection[index]
      this.removed.concept = removedConcept
      this.removed.type = type
      this.removed.index = index

      try {
        await ConceptApi.removeRelation(type, this.value.uuid, removedConcept.uuid)
        Global.alert({
          message: `"${removedConcept.name}" removed from ${this.removed.type}`,
          buttonText: 'Undo',
          buttonColor: 'primary',
          buttonCallback: this.undoRemoveConceptRelation
        })
        collection.splice(index, 1)
      } catch (err) {
        Global.alert({
          message: 'Deletion failed'
        })
      }
    },
    async undoRemoveConceptRelation () {
      if (!this.removed.concept) {
        return
      }
      try {
        await ConceptApi.addRelation(
          this.removed.type, this.value.uuid, this.removed.concept.uuid)
        this.value[this.removed.type].splice(this.removed.index, 0, this.removed.concept)
        Global.alert({
          message: 'Removal undone'
        })
      } catch (err) {
        Global.alert({
          messaeg: 'Undo failed'
        })
      }
    },
    async addConceptRelation (type) {
    }
  }
}
</script>
