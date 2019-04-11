import Vue from 'vue'
import Router from 'vue-router'
import HomeView from '@/views/HomeView'
import ConceptView from '@/views/ConceptView'

Vue.use(Router)

export default new Router({
  routes: [
    {
      path: '/',
      name: 'home',
      component: HomeView
    },
    {
      path: '/concept/:uuid',
      name: 'concept',
      component: ConceptView
    }
  ]
})
