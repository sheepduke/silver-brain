package silverbrain.item.domain

class ItemIdSuite extends munit.FunSuite:

  test("generate produces an id with i_ prefix"):
    val id = ItemId.generate()
    assert(id.value.startsWith("i_"))

  test("generate produces unique ids"):
    val id1 = ItemId.generate()
    val id2 = ItemId.generate()
    assertNotEquals(id1.value, id2.value)

  test("from succeeds for a valid id"):
    val id = ItemId.generate()
    assertEquals(ItemId.from(id.value), Right(id))

  test("from fails when prefix is missing"):
    val raw = "0ujtsYcgvSTl8PAuAdqWYSMnLOv"
    assert(ItemId.from(raw).isLeft)

  test("from fails for a wrong prefix"):
    val raw = "x_0ujtsYcgvSTl8PAuAdqWYSMnLOv"
    assert(ItemId.from(raw).isLeft)

  test("from fails when ksuid portion is invalid"):
    val invalid = "i_notaksuid"
    assert(ItemId.from(invalid).isLeft)

  test("from fails for empty string"):
    assert(ItemId.from("").isLeft)

  test("value returns the underlying string"):
    val id = ItemId.generate()
    assertEquals(id.value, id.value.stripPrefix(""))
    assert(id.value.startsWith("i_"))
