doc <- create_internal_doc("paper_diagram.bpmn")
repetition_log <- create_path_and_repetition_log(doc)
repetition_log_without_structured <- create_path_and_repetition_log(doc, FALSE)

doc_empty <- create_internal_doc("paper_diagram_empty.bpmn")
repetition_log_empty <- create_path_and_repetition_log(doc_empty)
length_path <- c(25, 14, 19, 25, 24, 25, 24, 14, 13, 14, 13, 19, 18, 19, 18, 25, 24,
                 24, 23, 14, 13, 13, 12, 19, 18, 18, 17)

path <- c("StartEvent_002tdck", "ExclusiveGateway_1wr5qja", "join", "Task_0e55sii", "ExclusiveGateway_1w6z1ap",
          "split", "Task_1qiqt0s", "ExclusiveGateway_1wr5qja", "join", "Task_0e55sii", "ExclusiveGateway_1w6z1ap",
          "split", "ExclusiveGateway_162lq5t", "join", "ExclusiveGateway_02r0dym", "split", "Task_1ehfjss", "Task_1ug8uhm",
          "join", "split", "Task_0d8b2ac", "ExclusiveGateway_16qlp2d", "join", "Task_0glvglb", "EndEvent_1nqeoua")

path_structured <- c("StartEvent_002tdck", "ExclusiveGateway_1wr5qja", "XOR-loop-join", "Task_0e55sii", "ExclusiveGateway_1w6z1ap",
          "XOR-loop-split", "Task_1qiqt0s", "ExclusiveGateway_1wr5qja", "XOR-loop-join", "Task_0e55sii", "ExclusiveGateway_1w6z1ap",
          "XOR-loop-split", "ExclusiveGateway_162lq5t", "start-join", "ExclusiveGateway_02r0dym", "AND-split", "Task_1ehfjss", "Task_1ug8uhm",
          "XOR-join", "AND-split", "Task_0d8b2ac", "ExclusiveGateway_16qlp2d", "AND-join", "Task_0glvglb", "EndEvent_1nqeoua")

context("path_log_creation")
test_that("path_log_creation", {
  expect_equal(length(repetition_log), 4)
  expect_equal(length(repetition_log_without_structured), 3)
  expect_equal(length(repetition_log_empty), 4)
  expect_equal(length(repetition_log[[1]]), 27)
  expect_equal(length(repetition_log_without_structured[[1]]), 27)
  expect_equal(length(repetition_log_empty[[1]]), 0)
  expect_equal(length(repetition_log[[2]]), 1)
  expect_equal(length(repetition_log_without_structured[[2]]), 1)
  expect_equal(length(repetition_log_empty[[2]]), 0)
  expect_equal(length(repetition_log[[3]]), 1)
  expect_equal(length(repetition_log_without_structured[[3]]), 1)
  expect_equal(length(repetition_log_empty[[3]]), 0)
  expect_equal(length(repetition_log[[4]]), 27)
  expect_equal(length(repetition_log_empty[[4]]), 0)
  expect_true(all.equal(repetition_log[[1]][[1]], path))
  expect_true(all.equal(repetition_log_without_structured[[1]][[1]], path))
  expect_true(all.equal(repetition_log[[4]][[1]], path_structured))
  expect_true(all.equal(sapply(repetition_log[[1]], length), length_path))
  expect_true(all.equal(sapply(repetition_log_without_structured[[1]], length), length_path))
  expect_true(all.equal(sapply(repetition_log[[2]], length), 8))
  expect_true(all.equal(sapply(repetition_log_without_structured[[2]], length), 8))
  expect_true(all.equal(sapply(repetition_log[[3]], length), 7))
  expect_true(all.equal(sapply(repetition_log_without_structured[[3]], length), 7))
  expect_true(all.equal(sapply(repetition_log[[4]], length), length_path))
  #expect_equal(loop_splits(repetition_log[[2]]), "ExclusiveGateway_1w6z1ap")
  #expect_equal(loop_joins(repetition_log[[3]]), "ExclusiveGateway_1wr5qja")
})
