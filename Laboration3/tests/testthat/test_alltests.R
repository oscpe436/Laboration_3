library(Laboration3)
context("Both functions")


test_that("gcd is calculated correctly in the euclidean algorithm.", {
  expect_equal(euclidean(123612, 13892347912), 4)
  expect_equal(euclidean(100, 1000), 100)
  expect_equal(euclidean(-100, 1000), 100)
})


test_that("Error messages are returned for erronous input in the euclidean algorithm.", {
  expect_error(euclidean("100", 1000))  
  expect_error(euclidean(100, "1000"))
  expect_error(euclidean("100", "1000"))  
})


test_that("outputs are correct in the Dijkstra algorithm.", {
  expect_equal(dijkstra(wiki_graph,1),c(0,7,9,20,20,11))
  expect_equal(dijkstra(wiki_graph,3),c(9,10,0,11,11,2))
})


test_that("Error messages are returned for erronous input in the Dijkstra algorithm.", {
  wiki_graph2 <- wiki_graph
  names(wiki_graph2) <- c("v1, v3, w")
  expect_error(dijkstra(wiki_graph2, 3))
  wiki_graph2 <- wiki_graph[1:2]
  expect_error(dijkstra(wiki_graph2, 3))
  expect_error(dijkstra(wiki_graph, 7))
  expect_error(dijkstra(as.matrix(wiki_graph), 3))  
})


