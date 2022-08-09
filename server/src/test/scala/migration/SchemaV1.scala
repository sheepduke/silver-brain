package silver_brain
package migration

case class ConceptV1(
    uuid: String,
    name: String,
    contentType: String,
    content: String,
    createTime: String,
    updateTime: String
)

case class ConceptLinkV1(
    source: String,
    target: String,
    createTime: String,
    updateTime: String
)
