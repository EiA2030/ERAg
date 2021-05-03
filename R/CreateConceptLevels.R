#' ERA Concept Hierarchy Tables
#'
#' Creates a list of data.tables describing the organizational structure of the ERA concept hierarchy for use in other ERA functions and interactive apps.
#'
#' @return CreateConceptLevels returns a list of four elements each a `data.table`:
#' * `Prac.Levels` Practice concept organizational levels
#' * `Out.Levels` Outcome concept organizational levels
#' * `Prod.Levels` Product (also called EU or experimental units) concept organizational levels
#' * `Agg.Levels` This data.table describes fields that can be used to aggregate data. `Observation` = no aggregation; each row of ERA is an observation, `Study` = the publication/study an
#' observation is reported from, and `Location` the spatial location of an observation (a combination of latitude, longitude and a buffer of uncertainty).
#' `Choice` and `Choice.Code` fields are simplified names/codes for hierarchical concept levels for user selection task. The remaining columns indicate the corresponding
#' column names in the ERA dataset for user selections.
#' @export
CreateConceptLevels<-function(){

  list(
    Prac.Levels=data.table(Choice=c("Subpractice","Practice"),
                            Choice.Code=c("S","P"),
                            Prac=c("SubPrName","PrName"),
                            Base=c("SubPrName.Base","PrName.Base")),

    Out.Levels=data.table(Choice=c("Subindicator","Indicator","Subpillar","Pillar"),
                           Choice.Code=c("SI","I","SP","P"),
                           Out=c("Out.SubInd","Out.Ind","Out.SubPillar","Out.Pillar")),

    Prod.Levels=data.table(Choice=c("Product","Subtype","Type"),
                            Choice.Code=c("P","S","T"),
                            Prod=c("Product.Simple","Product.Subtype","Product.Type")),

    Agg.Levels=data.table(Choice=c("Observation","Study","Location"),
                           Choice.Code=c("O","S","L"),
                           Agg=c("Index","Code","Site.Key"),
                           Label = c("No. Locations","No. Studies","No. Observations"))
  )

}
