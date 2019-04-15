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
            <concept-tag v-for="(parent, index) in value.parents"
                         :key="parent.uuid"
                         v-model="value.parents[index]"
                         close
                         @close="removeConceptRelation(Enum.Relation.PARENT, index)">
            </concept-tag>
            <v-btn small round
                   color="success"
                   @click="startAddConceptRelation(Enum.Relation.PARENT)">
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
              @click="startAddConceptRelation(Enum.Relation.CHILD)"
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
              @click="startAddConceptRelation(Enum.Relation.FRIEND)"
            >
              Add<v-icon right>add</v-icon>
            </v-btn>
          </p>
        </v-card-text>

        <v-divider></v-divider>

        <v-card-text>
          <v-card flat>
            <v-card-title>
              <h3>Content</h3>
              <v-btn v-if="!editConcept.enabled"
                     color="primary"
                     @click="editConcept.enabled = true">
                <v-icon>edit</v-icon>
              </v-btn>
              <v-btn v-else
                     color="success"
                     @click="updateConcept">
                <v-icon>save</v-icon>
              </v-btn>

              <v-btn color="error"
                     @click="deleteConcept.showDialog = true">
                <v-icon>delete</v-icon>
              </v-btn>
            </v-card-title>

            <v-card-text>
              <v-textarea v-model="value.content"
                          auto-grow autofocus
                          :readonly="!editConcept.enabled"></v-textarea>
            </v-card-text>

          </v-card>
        </v-card-text>
      </v-card>

      <v-dialog max-width="50%" v-model="newRelation.showDialog">
        <search-or-new-concept @select="addConceptRelation"
                               @close="newRelation.showDialog = false"
        ></search-or-new-concept>
      </v-dialog>

      <v-dialog max-width="50%" v-model="deleteConcept.showDialog">
        <v-card>
          <v-card-title>Confirmation</v-card-title>
          <v-card-text>
            <p>
              <v-icon>error</v-icon>
              This will delete this concept and all its relations.
            </p>
            <p>It cannot be undone. Do you confirm?</p>
            <v-btn color="error"
                   @click="doDeleteConcept">
              Confirm</v-btn>
            <v-btn @click="deleteConcept.showDialog = false">Cancel</v-btn>
          </v-card-text>
        </v-card>
      </v-dialog>
    </div>
  </div>
</template>

<script>
import ConceptTag from '@/components/ConceptTag'
import SearchOrNewConcept from '@/components/SearchOrNewConcept'
import * as ConceptApi from '@/api/concept'
import * as Enum from '@/util/enum'
import * as Global from '@/util/global'

export default {
  name: 'SingleConcept',
  components: {
    ConceptTag,
    SearchOrNewConcept
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
      newRelation: {
        showDialog: false,
        type: ''
      },
      editConcept: {
        enabled: false
      },
      deleteConcept: {
        showDialog: false
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
    async startAddConceptRelation (type) {
      this.newRelation.showDialog = true
      this.newRelation.type = type
    },
    async addConceptRelation (uuid) {
      if (uuid === this.value.uuid) {
        Global.alert({
          message: 'Cannot add itself',
          color: 'warning'
        })
        return
      }
      try {
        let type = this.newRelation.type

        if (this.value[type].find(c => c.uuid === uuid)) {
          Global.alert({
            message: `Concept already exists in ${type}`,
            color: 'warning'
          })
        } else {
          await ConceptApi.addRelation(type, this.value.uuid, uuid)
          let concept = await ConceptApi.getConceptByUuid(uuid)
          Global.alert({
            message: `Concept added to ${type}`,
            color: 'success'
          })
          this.value[type].push(concept)
          this.newRelation.showDialog = false
        }
      } catch (err) {
        Global.alert({
          message: 'Failed to add relation',
          color: 'error'
        })
      }
    },
    async updateConcept () {
      try {
        await ConceptApi.updateConcept(this.value)
        this.editConcept.enabled = false
        Global.alert({
          message: 'Concept updated',
          color: 'success'
        })
      } catch (err) {
        Global.alert({
          messaeg: 'Failed to update concept',
          color: 'error'
        })
      }
    },
    async doDeleteConcept () {
      try {
        await ConceptApi.deleteConcept(this.value.uuid)
        Global.alert({
          message: 'Concept deleted',
          color: 'success'
        })
        this.$router.push('/')
      } catch (err) {
        Global.alert({
          message: 'Failed to delete concept',
          color: 'error'
        })
      }
    }
  }
}
</script>
