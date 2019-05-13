import org.scalatest.FunSuite

class DagTest extends FunSuite  {

  test("DagLight"){

    val dag_0 = Dag(0, DagMode.Light)

    val dag_node_0_0 = dag_0(0)
    val node_0_0 = Array[Int](0x2922db22, 0x466c51cc, 0x860021d2, 0x7e41abf1, 0x82c3d10b, 0x6acc5e7c, 0x3fa3d3f7, 0x2b33ae8d, 0x1fa35aab, 0xcf718ec5, 0xe86676f2, 0x7718f41b, 0x9783745e, 0x419dca43, 0x4d51df0f, 0xc2ce9b00)
    assert(dag_node_0_0.sameElements(node_0_0))

    val dag_node_0_200 = dag_0(200)
    val node_0_200 = Array[Int](0xc4d08c6c, 0xec244cd8, 0xc90fdee7, 0x1b0dae70, 0xa0dbbc14, 0x50a064cf, 0xbc86dc9f, 0xa0e00b4d, 0x49649cfd, 0x5f9b6681, 0x502bca19, 0xae3e8f01, 0xea8e7d6a, 0xee05d0a5, 0xe9fe7db0, 0x3aa9d15f)
    assert(dag_node_0_200.sameElements(node_0_200))

    val dag_node_0_2000 = dag_0(2000)
    val node_0_2000 = Array[Int](0xf5d98db5, 0x2f4672d, 0x1cf52990, 0x52a05dbb, 0x85fceab8, 0x9bc701c8, 0x597b2662, 0x14b223c0, 0x6c806c7c, 0xb3ede483, 0x609481b7, 0xf6958ba5, 0x3a374eff, 0xc84c634c, 0x868ce464, 0xc0ebbaa3)
    assert(dag_node_0_2000.sameElements(node_0_2000))

    val dag_20 = Dag(20, DagMode.Light)

    val dag_node_20_0 = dag_20(0)
    val node_20_0 = Array[Int](0xf6145687, 0xb63719eb, 0x2ce1442a, 0x96c6aee4, 0xc6d99436, 0xd8030459, 0xd5739f9f, 0xaed11a29, 0x3256b246, 0x2357d326, 0xdeb26107, 0xef453ed4, 0x9af9a28b, 0x76e1d6a5, 0x1131734b, 0x79b02af8)
    assert(dag_node_20_0.sameElements(node_20_0))

    val dag_node_20_200 = dag_20(200)
    val node_20_200 = Array[Int](0xf025d6d9, 0xe235141e, 0x5fb41c29, 0x573ddce2, 0x8b1d7e, 0xf5f3022d, 0x962d5f2d, 0x76ca7e38, 0x281dd96a, 0x18a44d6, 0x1c3fa928, 0xa983a82e, 0xe9b82034, 0x34c41c56, 0xee392b52, 0x970f8a65)
    assert(dag_node_20_200.sameElements(node_20_200))

    val dag_node_20_2000 = dag_20(2000)
    val node_20_2000 = Array[Int](0xe798177d, 0xbc0910cb, 0x8308ae16, 0xcd143ae2, 0x40fb438c, 0x19600ad1, 0x99bdebbb, 0x2320af62, 0xb023ea82, 0x9ab0c2be, 0xfe9a0e13, 0xe9f8b03a, 0x7c8aa7e7, 0x5fe12c92, 0xf277f595, 0x6f5be488)
    assert(dag_node_20_2000.sameElements(node_20_2000))
  }

  test("DagFull"){

    val dag_0 = Dag(0, DagMode.Full)

    val dag_node_0_0 = dag_0(0)
    val node_0_0 = Array[Int](0x2922db22, 0x466c51cc, 0x860021d2, 0x7e41abf1, 0x82c3d10b, 0x6acc5e7c, 0x3fa3d3f7, 0x2b33ae8d, 0x1fa35aab, 0xcf718ec5, 0xe86676f2, 0x7718f41b, 0x9783745e, 0x419dca43, 0x4d51df0f, 0xc2ce9b00)
    assert(dag_node_0_0.sameElements(node_0_0))

    val dag_node_0_200 = dag_0(200)
    val node_0_200 = Array[Int](0xc4d08c6c, 0xec244cd8, 0xc90fdee7, 0x1b0dae70, 0xa0dbbc14, 0x50a064cf, 0xbc86dc9f, 0xa0e00b4d, 0x49649cfd, 0x5f9b6681, 0x502bca19, 0xae3e8f01, 0xea8e7d6a, 0xee05d0a5, 0xe9fe7db0, 0x3aa9d15f)
    assert(dag_node_0_200.sameElements(node_0_200))

    val dag_node_0_2000 = dag_0(2000)
    val node_0_2000 = Array[Int](0xf5d98db5, 0x2f4672d, 0x1cf52990, 0x52a05dbb, 0x85fceab8, 0x9bc701c8, 0x597b2662, 0x14b223c0, 0x6c806c7c, 0xb3ede483, 0x609481b7, 0xf6958ba5, 0x3a374eff, 0xc84c634c, 0x868ce464, 0xc0ebbaa3)
    assert(dag_node_0_2000.sameElements(node_0_2000))

    val dag_20 = Dag(20, DagMode.Full)

    val dag_node_20_0 = dag_20(0)
    val node_20_0 = Array[Int](0xf6145687, 0xb63719eb, 0x2ce1442a, 0x96c6aee4, 0xc6d99436, 0xd8030459, 0xd5739f9f, 0xaed11a29, 0x3256b246, 0x2357d326, 0xdeb26107, 0xef453ed4, 0x9af9a28b, 0x76e1d6a5, 0x1131734b, 0x79b02af8)
    assert(dag_node_20_0.sameElements(node_20_0))

    val dag_node_20_200 = dag_20(200)
    val node_20_200 = Array[Int](0xf025d6d9, 0xe235141e, 0x5fb41c29, 0x573ddce2, 0x8b1d7e, 0xf5f3022d, 0x962d5f2d, 0x76ca7e38, 0x281dd96a, 0x18a44d6, 0x1c3fa928, 0xa983a82e, 0xe9b82034, 0x34c41c56, 0xee392b52, 0x970f8a65)
    assert(dag_node_20_200.sameElements(node_20_200))

    val dag_node_20_2000 = dag_20(2000)
    val node_20_2000 = Array[Int](0xe798177d, 0xbc0910cb, 0x8308ae16, 0xcd143ae2, 0x40fb438c, 0x19600ad1, 0x99bdebbb, 0x2320af62, 0xb023ea82, 0x9ab0c2be, 0xfe9a0e13, 0xe9f8b03a, 0x7c8aa7e7, 0x5fe12c92, 0xf277f595, 0x6f5be488)
    assert(dag_node_20_2000.sameElements(node_20_2000))
  }
}
