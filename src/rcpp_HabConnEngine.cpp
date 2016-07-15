#include "../inst/include/Engine.h"
#include "../inst/include/Interface.h"
#include <R.h>
#include <Rinternals.h>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List habConnRcpp(NumericVector cost, int nrow, int ncol, double hab, double no_data, double increment = 1.0)
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
	//other properties of input data
	in_data.nrow = nrow;
	in_data.ncol = ncol;
	in_data.habitat = (float)hab;
	in_data.nodata = (float)no_data;

	//call the interface function CalcEngine
	bool success = CalcEngine(in_data, out_data, (float)increment);
	if (!success)
	{
		Rprintf("Engine not successful.\n");
		return R_NilValue;
	}
	
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

	//create the returned data structure
	List link_data_vec(out_data.link_data.size());
	for (unsigned int i = 0; i < out_data.link_data.size(); i++)
	{
		link_data_vec[i] = List::create(Named("StartId", out_data.link_data[i].start.id),
			Named("StartRow", out_data.link_data[i].start.row),
			Named("StartColumn", out_data.link_data[i].start.column),
			Named("EndId", out_data.link_data[i].end.id),
			Named("EndRow", out_data.link_data[i].end.row),
			Named("EndColumn", out_data.link_data[i].end.column),
			Named("Cost", out_data.link_data[i].cost));
	}
	



	return List::create(Named("VoronoiVector", nmvor),
		Named("PatchVector", nmpatch),
		Named("LinkVector", nmlink),
		Named("LinkData", link_data_vec));
}


/*
SEXP HabConnEngine(SEXP cost, SEXP nrow, SEXP ncol, SEXP hab, SEXP no_data, SEXP voronoi_map, SEXP link_map, SEXP patch_map)
{
  Rprintf("************* Engine Called ***************\n");
  int ncost = length(cost);
  int c_hab, c_no_data, c_ncol, c_nrow;
  c_hab = INTEGER(hab)[0];
  c_no_data = INTEGER(no_data)[0];
  c_ncol = INTEGER(ncol)[0];
  c_nrow = INTEGER(nrow)[0];

  Rprintf("Verifying inputs\n");
  Rprintf("Number of rows: %d\nNumber of columns: %d\nHabitat value: %d\nNo data value: %d\n", c_nrow, c_ncol, c_hab, c_no_data);

  vector<int> c_cost(ncost, 0);
  for (unsigned int i = 0; i < c_cost.size(); i++)
  {
    c_cost[i] = REAL(cost)[i];
  }

  OutputData out_d;
  InputData in_d;
  in_d.cost_vec = c_cost;
  in_d.nrow = c_nrow;
  in_d.ncol = c_ncol;
  in_d.habitat = c_hab;
  in_d.nodata = c_no_data;

  Rprintf("Engine started\n");
  bool success = CalcEngine(in_d, out_d, 1.0f);
  if (!success)
    return NILSXP;
  SEXP ret;

  //get voronoi map
  Rprintf("Grabbing the voronoi vector\n");

  for (unsigned int i = 0; i < out_d.voronoi_map.size(); i++)
  {
    REAL(voronoi_map)[i] = out_d.voronoi_map[i];
  }

  //get link map
  Rprintf("Grabbing the link vector\n");

  for (unsigned int i = 0; i < out_d.link_map.size(); i++)
  {
    REAL(link_map)[i] = out_d.link_map[i];
  }

  //get patch map
  Rprintf("Grabbing the patch vector\n");

  for (unsigned int i = 0; i < out_d.patch_map.size(); i++)
  {
    REAL(patch_map)[i] = out_d.patch_map[i];
  }

  //return the link data in matrix form
  //rows are the links
  //column 1 = start patch id
  //column 2 = start row
  //column 3 = start column
  //column 4 = end patch id
  //column 5 = end row
  //column 6 = end column
  //column 7 = cost

  Rprintf("Number of links: %d\n", out_d.link_data.size());
  PROTECT(ret = allocMatrix(REALSXP, out_d.link_data.size(), 7));
  double * p_ret = REAL(ret);
  for (unsigned int i = 0; i < out_d.link_data.size(); i++)
  {
    p_ret[i + 27 * 0] = (int)out_d.link_data[i].start.id;
    p_ret[i + 27 * 1] = (int)out_d.link_data[i].start.row;
    p_ret[i + 27 * 2] = (int)out_d.link_data[i].start.column;
    p_ret[i + 27 * 3] = (int)out_d.link_data[i].end.id;
    p_ret[i + 27 * 4] = (int)out_d.link_data[i].end.row;
    p_ret[i + 27 * 5] = (int)out_d.link_data[i].end.column;
    p_ret[i + 27 * 6] = (int)out_d.link_data[i].cost;
  }
  UNPROTECT(1);
  Rprintf("Engine Finished\n\n\n");
  return ret;
}*/
