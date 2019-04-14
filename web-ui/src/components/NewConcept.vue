<!--
     A card that contains form for creating a concept.

     Emits:
     * success (uuid): when the UUID of the new concept has been created.
     * fail: when the creation of new concept failed.
     * close: when close button is clicked.
-->

<template>
  <div id="new-concept">
    <v-card>

      <v-card-title v-if="title && title.length > 0">{{ title }}</v-card-title>

      <v-card-text>
        <v-text-field
          v-model="name"
          label="Concept Name"
        ></v-text-field>
      </v-card-text>

      <v-card-text>
        <v-select
          v-model="contentFormat"
          :items="contentFormatList"
          label="Format of content"
        ></v-select>
      </v-card-text>

      <v-card-text>
        <v-textarea
          v-model="content"
          label="Content"
          solo
        ></v-textarea>
      </v-card-text>

      <v-card-text>
        <v-btn
          flat
          color="success"
          @click="submit()"
        >Submit</v-btn>

        <v-btn flat
               @click="close()"
        >Close</v-btn>
      </v-card-text>
    </v-card>
  </div>
</template>

<script>
import * as ConceptApi from '@/api/concept'
import * as Global from '@/util/global'

export default {
  name: 'NewConcept',
  data () {
    return {
      name: '',
      content: '',
      contentFormatList: ['plain', 'org', 'markdown'],
      contentFormat: 'plain'
    }
  },
  props: {
    title: {
      type: String,
      default: ''
    }
  },
  methods: {
    reset () {
      this.name = ''
      this.content = ''
      this.contentFormat = 'plain'
    },
    async submit () {
      if (this.name.length === 0) {
        Global.alert({
          message: 'Concept name cannot be empty',
          color: 'warning'
        })
      } else {
        try {
          let url = await ConceptApi.newConcept(this.name, this.content, this.contentFormat)
          Global.alert({
            message: `New concept "${this.name}" created`,
            color: 'success'
          })
          this.$emit('success', url)
        } catch (err) {
          Global.alert({
            message: `Failed to create concept "${name}"`,
            color: 'error'
          })
          this.$emit('fail')
        }
        this.reset()
      }
    },
    close () {
      this.reset()
      this.$emit('close')
    }
  }
}
</script>
