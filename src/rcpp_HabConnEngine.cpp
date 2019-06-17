#define R_NO_REMAP

#include "../inst/include/Engine.h"
#include <R.h>
#include <Rinternals.h>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;
//' Habitat connectivity engine (C++)
//'
//' Internal function, not intended to be called directly.
//' DESCRIPTION NEEDED.
//'
//' @param cost              Numeric vector of habitat cost (resistance) values
//'                          extracted from a raster cost map.
//'
//' @param patches           Numeric vector that have binary values (\code{0} and \code{1})
//'                          where ones corresponds to patch cells and zeroes to
//'                          non-habitat (i.e., matrix) cells.
//'
//' @param nrow              Number of rows in the raster cost/patch map.
//'
//' @param ncol              Number of columns in the raster cost/patch map.
//'
//' @author Sam Doctolero
//' @keywords internal
//' @rdname habConnRcpp
// [[Rcpp::export(name = ".habConnRcpp")]]
List habConnRcpp(NumericVector cost, NumericVector patches, int ncol, int nrow)
{
  //create instances of inputdata and output data
  InputData in_data;
  OutputData out_data;

  //initialize the input data cost vector
  in_data.cost_vec.resize(cost.size());
  for (unsigned int i = 0; i < in_data.cost_vec.size(); i++)
  {
    in_data.cost_vec[i] = (float)cost[i];
  }

  //initialize the input data patches vector
  in_data.patch_vec.resize(patches.size());
  for (unsigned int i = 0; i < in_data.patch_vec.size(); i++)
  {
	  in_data.patch_vec[i] = (float)patches[i];
  }

  //other properties of input data
  in_data.nrow = nrow;
  in_data.ncol = ncol;

  char error_msg[MAX_CHAR_SIZE] = {};

  Engine habConnCalculator(&in_data, &out_data, error_msg, MAX_CHAR_SIZE);
  if (!habConnCalculator.initialize())
  {
    Rprintf("Engine did not initialize due to %s\n", error_msg);
    return R_NilValue;
  }
  habConnCalculator.start();

  //create NumericVector values for vector values of out_data
  NumericVector nmvor(cost.size());
  NumericVector nmpatch(cost.size());

  //transfer the vector data to the numericvector variables
  for (unsigned int i = 0; i < cost.size(); i++)
  {
    nmvor[i] = (double)out_data.voronoi_map[i];
    nmpatch[i] = (double)out_data.patch_map[i];
  }

  // create the returned data structure (R indexes from 1, so add 1 to the row/col values)
  List link_data_vec(out_data.link_data.size());
  for (unsigned int i = 0; i < out_data.link_data.size(); i++)
  {
    link_data_vec[i] = List::create(Named("LinkId", (double)(i + 1)*(-1.0)),
      Named("StartId", out_data.link_data[i].start.id),
      Named("StartRow", out_data.link_data[i].start.row+ 1),
      Named("StartColumn", out_data.link_data[i].start.column + 1),
      Named("EndId", out_data.link_data[i].end.id),
      Named("EndRow", out_data.link_data[i].end.row + 1),
      Named("EndColumn", out_data.link_data[i].end.column + 1),
      Named("PerimWeight", out_data.link_data[i].cost));

    //go through each cell in the link data and put in their values
    for (unsigned int j = 0; j < out_data.link_data[i].connection.size(); j++)
    {
      int row = out_data.link_data[i].connection[j].row;
      int col = out_data.link_data[i].connection[j].column;
      nmpatch[row*in_data.ncol + col] = (double)(i + 1)*(-1.0);
    }
  }

  return List::create(Named("VoronoiVector", nmvor),
                      Named("PatchLinkIDsVector", nmpatch),
                      Named("LinkData", link_data_vec));
}
