<template>
  <v-chip
    :close="close"
    @input="$emit('close')"
  >
    <a :href="`#/concept/${value.uuid}`">{{ value.name }}</a>
  </v-chip>
</template>

<script>
export default {
  name: 'ConceptTag',
  props: {
    value: {
      type: Object,
      required: true
    },
    close: {
      type: Boolean,
      default: true
    }
  }
}
</script>
