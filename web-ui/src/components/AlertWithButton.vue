<template>
  <div>
    <v-snackbar
      :value="value"
      @input="close"
      :color="color"
      top
    >
      {{ message }}

      <v-btn
        v-if="buttonText"
        flat
        :color="buttonColor"
        @click="click"
      >
        {{ buttonText }}
      </v-btn>

      <v-btn
        fab flat
        @click="close"
      >
        <v-icon dark>close</v-icon>
      </v-btn>
    </v-snackbar>

  </div>
</template>

<script>
export default {
  name: 'AlertWithButton',
  props: {
    value: {
      required: true
    },
    message: {
      type: String,
      required: true
    },
    color: {
      type: String,
      default: ''
    },
    buttonText: {
      type: String
    },
    buttonColor: {
      type: String
    }
  },
  methods: {
    click() {
      this.$emit('click')
    },
    close() {
      this.$emit('close')
    }
  }
}
</script>
