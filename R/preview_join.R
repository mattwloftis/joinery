
# preview joins

preview_join <- function(x, y, by = NULL, by.x = NULL, by.y = NULL,
                         time_x = NULL, time_y = NULL) {
                         #join_type = c("inner", "left", "right", "full")) {

  # are any 'by' variables specified??
  any_by <- !(is.null(by) & is.null(by.x) & is.null(by.y))

  # if is.null(by) --> get intersect var names
  if (!any_by) {
    by <- intersect(names(x), names(y))
  }

  # warn if no intersecting names & is.null(by)
  if (!any_by && length(by) == 0) {
    stop("Datasets contain no common variable names./nPlease specify 'by'
         option to indicate variables to merge on.")
  }

  # get additional names
  xnume_extra <- names(x)[!(names(x) %in% c(by, by.x))]
  ynume_extra <- names(y)[!(names(y) %in% c(by, by.y))]


  ## DEAL W/ JOINING VARIABLS
  # check name intersects (esp. for is.null(by) = TRUE)
  if (!any_by) {

  }

  # check missingness on join variables

  # if !is.null(by) --> check for name conflicts


  ## DEAL W/ TIME
  # check missingness on time variables

  # check consistent formatting of time vars

  # check that time granularity harmonizes

  # in case of conflicting time granularity: recommend (dis)aggregations (???)


  ## DEAL W/ OBSERVATIONS
  # check how many y vars will drop in left_join
  if (join_type %in% c("inner", "left")) {

  }

  # check how many x vars will drop in right_join


  # check how many x & y vars will drop in inner_join


  # check size of final data under alternative join types


  # PREP FOR S3 PRINT METHOD TO SHOW
    # MISSINGNESS ON GROUPING/TIME VARS
    # OB INCLUSION BY JOIN TYPES?

  # return S3 object with print method

}
