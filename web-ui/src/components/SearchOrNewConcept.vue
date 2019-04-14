<template>
  <v-card>
    <v-card-title>New Relation From...</v-card-title>
    <v-card-text>
      <v-tabs>
        <v-tab>Existing</v-tab>
        <v-tab>New</v-tab>

        <v-tab-item>
          <search-concept @select="emitSelected"
          ></search-concept>
        </v-tab-item>

        <v-tab-item>
          <new-concept :no-actions="true"
                       @success="emitSelected"
                       @close="emitClose"
          ></new-concept>
        </v-tab-item>
      </v-tabs>
    </v-card-text>
  </v-card>
</template>

<script>
import NewConcept from '@/components/NewConcept'
import SearchConcept from '@/components/SearchConcept'

export default {
  name: 'SearchOrNewConcept',
  components: {
    NewConcept,
    SearchConcept
  },
  methods: {
    emitSelected (uuid) {
      this.$emit('select', uuid)
    },
    emitClose () {
      this.$emit('close')
    }
  }
}
</script>
