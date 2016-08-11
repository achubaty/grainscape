#define R_NO_REMAP

#include "../inst/include/Engine.h"
#include <R.h>
#include <Rinternals.h>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List habConnRcpp(NumericVector cost, int nrow, int ncol, double hab, double no_data, NumericVector distinctValues,double threshold = 0.0001)
{
  //create instances of inputdata and output data
  InputData in_data;
  OutputData out_data;
  //initialize the input data
  //cost vector
  in_data.cost_vec.resize(cost.size());
  for (unsigned int i = 0; i < in_data.cost_vec.size(); i++)
  {
    in_data.cost_vec[i] = cost[i];
  }
  //distinct values
  in_data.distinctValues.resize(distinctValues.size());
  for (unsigned int i = 0; i < in_data.distinctValues.size(); i++)
  {
    in_data.distinctValues[i] = distinctValues[i];
  }

  //other properties of input data
  in_data.nrow = nrow;
  in_data.ncol = ncol;
  in_data.habitat = (float)hab;
  in_data.nodata = (float)no_data;

  //char error_msg[MAX_CHAR_SIZE] = "Pass in this variable to the engine as an error message holder\n";
  char error_msg[MAX_CHAR_SIZE] = "success\n";
  Engine habConnCalculator(&in_data, &out_data, error_msg, (float)threshold);
  if (!habConnCalculator.initialize())
  {
    Rprintf("Engine did not initialize due to %s\n", error_msg);
    return R_NilValue;
  }
  habConnCalculator.start();
  Rprintf("%s\n", error_msg);

  //create NumericVector values for vector values of out_data
  NumericVector nmvor(cost.size());
  NumericVector nmlink(cost.size());
  NumericVector nmpatch(cost.size());
  //transfer the vector datat to the numericvector variables
  for (unsigned int i = 0; i < cost.size(); i++)
  {
    nmvor[i] = (double)out_data.voronoi_map[i];
    nmlink[i] = (double)out_data.link_map[i];
    nmpatch[i] = (double)out_data.patch_map[i];
  }

  //create numeric vectors for lcpPerimWeight and lcpLinkId
  NumericVector lcpPW(cost.size());  //numericvector variable for lcpPerimWeight map
  NumericVector lcpLI(cost.size());  //numericvector variable for lcpLinkId

  //create the returned data structure
  List link_data_vec(out_data.link_data.size());
  for (unsigned int i = 0; i < out_data.link_data.size(); i++)
  {
    link_data_vec[i] = List::create(Named("LinkId", i+1),
      Named("StartId", out_data.link_data[i].start.id),
      Named("StartRow", out_data.link_data[i].start.row),
      Named("StartColumn", out_data.link_data[i].start.column),
      Named("EndId", out_data.link_data[i].end.id),
      Named("EndRow", out_data.link_data[i].end.row),
      Named("EndColumn", out_data.link_data[i].end.column),
      Named("PerimWeight", out_data.link_data[i].cost));

    //go through each cell in the link data and put in their values
    for (unsigned int j = 0; j < out_data.link_data[i].connection.size(); j++)
    {
      int row = out_data.link_data[i].connection[j].row;
      int col = out_data.link_data[i].connection[j].column;
      lcpPW[row*in_data.ncol + col] = out_data.link_data[i].cost;
      lcpLI[row*in_data.ncol + col] = i;
    }
  }

  return List::create(Named("VoronoiVector", nmvor),
    Named("PatchVector", nmpatch),
    Named("LinkVector", nmlink),
    Named("lcpPerimWeight", lcpPW),
    Named("lcpLinkId", lcpLI),
    Named("LinkData", link_data_vec));
}
