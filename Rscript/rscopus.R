# institutions
  require(rscopus)
options("elsevier_api_key" = "662b78ed7e0ab4f39a45177b026a79da")

if (have_api_key()) {
  res = author_df(last_name = "Muschelli", first_name = "John",
                  verbose = FALSE)
}
get_api_key(api_key = "662b78ed7e0ab4f39a45177b026a79da", error = TRUE)
print("662b78ed7e0ab4f39a45177b026a79da",T)



if (have_api_key()) {
  res = get_complete_author_info(
    last_name = "Muschelli",
    first_name = "John",
    verbose = FALSE)
}

