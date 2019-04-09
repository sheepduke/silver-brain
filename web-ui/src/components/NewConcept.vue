<template>
  <div id="new-concept">
    <v-card>

      <v-card-text>
        <v-text-field
          v-model="name"
          label="Concept Name"
          @input="alert.show = false"
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
        <v-alert
          v-model="alert.show"
          dismissible
          type="warning"
        >{{ alert.text }}</v-alert>
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
export default {
  name: 'NewConcept',
  data () {
    return {
      name: '',
      content: '',
      contentFormatList: ['plain', 'org', 'markdown'],
      contentFormat: 'plain',
      alert: {
        show: false,
        text: ''
      }
    }
  },
  methods: {
    reset () {
      this.name = ''
      this.content = ''
      this.contentFormat = 'plain'
      this.alert.show = false
      this.alert.text = ''
    },
    submit () {
      if (this.name.length === 0) {
        this.alert.text = 'Concept name cannot be empty'
        this.alert.show = true
      } else {
        this.$emit('submit', this.name, this.content, this.contentFormat)
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
