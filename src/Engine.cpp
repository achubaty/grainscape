#include "../inst/include/Engine.h"
#include <Rcpp.h> // comment this when NOT using R

//' Engine constructor
//'
//' Sets the values for its internal variables.
//'
//' @author Sam Doctolero
//' @keywords internal
Engine::Engine(InputData * in_d, OutputData * out_d, char * errmsg, int size_err) {
  // give input data's pointer (or the address of what it is pointing to)
  in_data = in_d;

  // give output data's pointer (or the address of what it is pointing to)
  out_data = out_d;

  // will set this to true once engine initialized
  initialized = false;

  // create a map with floating point zero in each cell for the voronoi map
  voronoi_map = flMap(in_d->nrow, flCol(in_d->ncol, 0.0f));

  // create a map with floating point zero in each cell for the cost/resistance map
  cost_map = flMap(in_d->nrow, flCol(in_d->ncol, 0.0f));

  // give the errmsg's pointer value to the error_message variable
  error_message = errmsg;

  // give the size_err to error_message_size
  error_message_size = size_err;
}

//' Default Engine constructor
//'
//' @author Sam Doctolero
//' @keywords internal
Engine::Engine() {
  in_data = 0;          // give the pointer a null
  out_data = 0;         // give the pointer a null
  initialized = false;  // initialized is set to false
  maxCost = 0.0f;       // let maxCost be zero
}

//' Default Engine destructor
//'
//' @author Sam Doctolero
//' @keywords internal
Engine::~Engine() {
  // doesn't do anything, since there aren't any dynamic memory allocations.
}

//' Initialize the engine
//'
//' This function should be called before running the engine.
//'
//' @author Sam Doctolero
//' @keywords internal
bool Engine::initialize() {
  // check to see if the input vector is equal to the number of cells in a map
  unsigned int size = in_data->nrow * in_data->ncol;

  if (size != in_data->cost_vec.size()) {
    char msg[] = "product of number of rows and columns did not match cost/resistance vector size\n";
    writeErrorMessage(msg,strlen(msg));
    return false;
  }

  // initialize maxCost and costRes
  // (maxCost is the max resistance value in the raster and costRes is the minimum)
  costRes = in_data->cost_vec[0];
  maxCost = in_data->cost_vec[0];
  for (unsigned int i = 1; i < in_data->cost_vec.size(); i++) {
    // if the i'th element of cost_vec is smaller than costRes then give that to costRes
    if (costRes > in_data->cost_vec[i])
      costRes = in_data->cost_vec[i];

    // if the i'th element of cost_vec is larger than maxCost then give that to maxCost
    if (maxCost < in_data->cost_vec[i])
      maxCost = in_data->cost_vec[i];
  }

  // insert the cost values in the actual cost map
  for (unsigned int i = 0; i < cost_map.size(); i++) {
    for (unsigned int j = 0; j < cost_map[0].size(); j++) {
      cost_map[i][j] = (float)in_data->cost_vec[i*in_data->ncol + j];
    }
  }

  // find the patches (creates initial voronoi_map)
  findPatches();

  // update the output patch vector
  updateOutputMap(out_data->patch_map, voronoi_map);

  // get the initial active/spread cells
  for (int i = in_data->nrow - 1; i >= 0; i--) {
    for (int j = in_data->ncol - 1; j >= 0; j--) {
      if (voronoi_map[i][j] > 0.0f) {
        bool isActive = false;
        //look at all the 4 adjacent cells:
        // left
        isActive = cellIsZero(i, j - 1);
        // right
        if (!isActive && cellIsZero(i, j + 1))
          isActive = true;
        // bottom
        if (!isActive && cellIsZero(i + 1, j))
          isActive = true;
        // top
        if (!isActive && cellIsZero(i - 1, j))
          isActive = true;

        // if the cell is supposed to be an active cell then add it to the
        // active_cell_holder internal variable/property of the engine object
        if (isActive) {
          ActiveCellHolder holder_t;             // instance of an ActiveCellHolder called 'holder_t'
          holder_t.value = 0.0f;                 // set the property "value" to zero

          Cell c;                                // create an instance of a Cell called 'c'
          c.row = i;                             // set the row variable of Cell 'c' to i's current value
          c.column = j;                          // set the column variable of Cell 'c' to 'j' current value
          c.id = (voronoi_map[i][j]);            // give Cell 'c' an id, corresponding to voronoi map's i'th and j'th element

          ActiveCell ac;                         // create an instance of an ActiveCell called 'ac'
          ac.time = 0.0f;                        // set the time inside 'ac' to zero
          ac.id = c.id;                          // set the id of 'ac' to 'c' id
          ac.distance = 0.0f;                    // set the Euclidean distance from this element/cell to its origin element/cell to to zero
          ac.resistance = cost_map[i][j];        // set the resistance of 'ac' to the value in the cost map's i'th and j'th element
          ac.originCell = c;                     // set the origin cell of 'ac' to c's properties
          ac.row = i;                            // set ac's row to i's value
          ac.column = j;                         // set ac's column to i's value
          ac.parentResistance = 0.0f;            // set the parentResistance to zero

          holder_t.add(ac);                      // add the ActiveCell 'ac' to the ActiveCellHolder 'holder_t' by calling the add function
          active_cell_holder.insertH(holder_t);  // insert holder_t in the active_cell_holder variable of the engine object
        }
      }
    }
  }

  // if the initial active cell holder is zero then there's no need for the engine to start
  if (active_cell_holder.size() <= 0) {
    char msg[] = "no initial active cells found\n";
    writeErrorMessage(msg,strlen(msg));
    return false;
  }

  // resize link map (nrow rows and ncol columns)
  iLinkMap = LinkMap(in_data->nrow, lcCol(in_data->ncol));

  // initialize the LinkCells in iLinkMap to some default value,
  // as the for loop below only initializes active cells
  for (unsigned int i = 0; i < in_data->nrow; i++) {
    for (unsigned int j = 0; j < in_data->ncol; j++) {
      iLinkMap[i][j].row = i;
      iLinkMap[i][j].column = j;
      iLinkMap[i][j].fromCell.row = i;       // will be set properly for active cells below
      iLinkMap[i][j].fromCell.column = j;    // will be set properly for active cells below
      iLinkMap[i][j].originCell.row = i;     // will be set properly for active cells below
      iLinkMap[i][j].originCell.column = j;  // will be set properly for active cells below
    }
  }

  // parse through each ActiveCell in the active_cell_holder
  for (unsigned int i = 0; i < active_cell_holder.holder_list[0].size(); i++) {
    // at this point the active_cell_holder will only have one list element
    ActiveCell ac = active_cell_holder.holder_list[0].list[i];  // grab the i'th ActiveCell in the list and call it 'ac'

    LinkCell lc;                       // create an instance of a LinkCell called 'lc'
    lc.row = ac.row;                   // set lc's row to ac's row
    lc.column = ac.column;             // set lc's column to ac's column
    lc.id = ac.id;                     // set lc's id to ac's id
    lc.fromCell = ac.originCell;       // set lc's fromCell property to ac's originCell property
    lc.originCell = ac.originCell;     // set lc's originCell property to ac's originCell property
    lc.distance = 0.0f;                // set the Euclidean distance to zero
    lc.cost = 0.0f;                    // set the cost to zero

    iLinkMap[lc.row][lc.column] = lc;  // insert lc to the appropriate element in the iLinkMap property of the engine
  }

  // once all the initialization parameters are done set the initialized property to true
  initialized = true;

  return initialized;
}

//' Start the engine
//'
//' @author Sam Doctolero
//' @keywords internal
void Engine::start() {
  // if the engine object is not properly initialized then do not run the engine and just return
  if (!initialized) {
    char msg[] = "Engine is not initialized. Failed";
    writeErrorMessage(msg, strlen(msg));
    return;
  }

  // keep looping until there aren't any more active cells
  while (active_cell_holder.size() > 0) {
    Rcpp::checkUserInterrupt();  // comment this when NOT using R

    temporary_active_cell_holder.holder_list.clear(); // clear the temporary active cell holder

    // go through each active cell to see if any of them are ready to spread:
    //   if they are ready to spread then add them to the spread_list;
    //   if not then include that active cell in the temporary active cell holder for later.
    for (unsigned int i = 0; i < active_cell_holder.size(); i++) {
      std::vector<ActiveCell> ac_to_check = active_cell_holder.holder_list[i].list;
      for (unsigned int j = 0; j < ac_to_check.size(); j++) {
        activeCellSpreadChecker(&ac_to_check[j]);
      }
    }

    // go through each spreading cell and check all the adjacent cells if they can be conquered
    for (unsigned int i = 0; i < spread_list.size(); i++) {
      createActiveCell(&spread_list[i], spread_list[i].row - 1, spread_list[i].column);  // top
      createActiveCell(&spread_list[i], spread_list[i].row + 1, spread_list[i].column);  // bottom
      createActiveCell(&spread_list[i], spread_list[i].row, spread_list[i].column + 1);  // right
      createActiveCell(&spread_list[i], spread_list[i].row, spread_list[i].column - 1);  // left
    }

    spread_list.clear(); // clear the spread list

    active_cell_holder = temporary_active_cell_holder; // set the new active cells
  }

  // fill the output's voronoi vector with the engine's voronoi_map values
  updateOutputMap(out_data->voronoi_map, voronoi_map);
}

//' Update the output map
//'
//' @author Sam Doctolero
//' @keywords internal
void Engine::updateOutputMap(std::vector<float> & vm, flMap mm) {
  // resize vm and allocate nrow*ncol number of floating point elements
  vm = std::vector<float>(in_data->nrow*in_data->ncol, 0);

  // parse through each element in the map 'mm' that has floating point values in it
  // and assign those values to the appropriate element in the 'vm' vector
  for (unsigned int i = 0; i < in_data->nrow; i++) {
    for (unsigned int j = 0; j < in_data->ncol; j++) {
      vm[i*in_data->ncol + j] = mm[i][j];
    }
  }
}

//' Check whether the value of a cell is zero
//'
//' If the row and column values are not out of bounds and the voronoi_map's
//' \code{row}'th and \code{col}'th element is zero, then return \code{true}.
//'
//' @author Sam Doctolero
//' @keywords internal
bool Engine::cellIsZero(int row, int col) {
  if (!outOfBounds(row, col, in_data->nrow, in_data->ncol) && !(voronoi_map[row][col] > 0.0f)) {
    return true;
  }
  return false;
}

//' Check the spread status of an \code{ActiveCell}
//'
//' Assign the current \code{ActiveCell} to either \code{spread_list} or
//' \code{temporary_active_cell_holder}.
//'
//' @param ac \code{ActiveCell} pointer
//'
//' @return Nothing.
//'         As a side effect, updates \code{spread_list} and \code{temporary_active_cell_holder}.
//'
//' @author Sam Doctolero
//' @keywords internal
void Engine::activeCellSpreadChecker(ActiveCell * ac) {
  // if the time or number of iteration since the ActiveCell is instantiated is
  // greater than the resistance value at that cell, include it in the spread_list.
  if (ac->time >= ac->resistance) {
    // include the list in spread list in an order by increasing distance
    if (spread_list.size() <= 0) {
      spread_list.push_back(*ac);
    } else {
      // the active cell with the shortest Euclidean distance should be placed up front
      // (the spread_list is kept sorted in increasing order of Euclidean distance)
      int index = 0;
      for (int i = spread_list.size() - 1; i >= 0; i--) {
        if (spread_list[i].distance <= ac->distance) {
          index = i + 1;
          break;
        }
      }
      spread_list.insert(spread_list.begin() + index, *ac);
    }
  } else {
    // if the time difference is still smaller than the resistance value
    // include it in the temporary_active_cell_holder
    // find the proper queue that the active cell belongs to

    // increment the time property by either 1.0 or the equation below (whichever is the largest)
    // NOTE: maxCost is the maximum cost on the raster; costRes is the minimum.
    ac->time += std::max(1.0f, (ac->resistance - ac->parentResistance) * costRes / maxCost);

    ActiveCellHolder h_temp;       // create an instance of an ActiveCellHolder called 'h_temp'
    h_temp.value = ac->distance;  // set h_temp's value to ac's distance
    h_temp.list.push_back(*ac);    // insert the ActiveCell that ac is pointing to
                                   // at the end of h_temp's list property (a vector of ActiveCells)

    // insert h_temp in the temporary_active_cell_holder (keeping it sorted)
    temporary_active_cell_holder.insertH(h_temp);
  }
}

//' Create a new \code{ActiveCell}
//'
//' @author Sam Doctolero
//' @keywords internal
void Engine::createActiveCell(ActiveCell * ac, int row, int col) {
  // if not out of bounds and have not been conquered by other patches then create a new active cell
  if (!outOfBounds(row, col, in_data->nrow, in_data->ncol) && voronoi_map[row][col] == 0.0f) {
    // handle no-data values
    if (std::isnan(cost_map[row][col])) {
      voronoi_map[row][col] = std::numeric_limits<float>::quiet_NaN();
      return;
    }

    Cell c;          // create an instance of Cell called 'c'
    c.row = row;     // set c's row to the parameter row
    c.column = col;  // set c's column to the parameter col
    c.id = ac->id;   // set c's id to the id of the ActiveCell that 'ac' is pointing to

    // create a variable 'dist' and calculate the distance between 'c' and ac's originCell
    float dist = calcDistance(ac->originCell, c);

    ActiveCell new_ac;                         // create an instance of ActiveCell called 'new_ac'
    new_ac.row = row;                          // set new_ac's row to c's row
    new_ac.column = col;                       // set new_ac's column to c's column
    new_ac.id = c.id;                          // set new_ac's id to c's id
    new_ac.distance = dist;                    // set new_ac's distance to dist
    new_ac.originCell = ac->originCell;        // set new_ac's originCell to ac's originCell
    new_ac.parentResistance = ac->resistance;  // set new_ac's parentResistance to ac's resistance
    new_ac.resistance = cost_map[row][col];    // set new_ac's resistance to cost_map's row'th and col'th element
    new_ac.time = 0.0f;                        // set the time to zero in new_ac

    // this is the actual spreading move
    voronoi_map[row][col] = ac->id;  // then set voronoi_map's row'th and col'th element to ac's id

    // create link between old active cell (ac) and the cell that it is spreading to (new_ac)
    connectCell(ac, row, col, cost_map[row][col]);

    // insert new_ac to the temporary_active_cell_holder as a new ActiveCell
    ActiveCellHolder h_temp;           // create an instance of ActiveCellHolder called 'h_temp'
    h_temp.value = dist;            // set h_temp's value to dist
    h_temp.list.push_back(new_ac);     // insert new_ac into h_temp's list property

    // insert the temporary_active_cell holder (keep properly sorted)
    temporary_active_cell_holder.insertH(h_temp);
  }

  // create the links
  // check if the row and col arguments are not out of bounds,
  //   voronoi_map's row'th and col'th element is not zero,
  //   and not ac's id, then this is a voronoi boundary
  if (!outOfBounds(row, col, in_data->nrow, in_data->ncol) &&
      voronoi_map[row][col] != 0.0f &&
      voronoi_map[row][col] != ac->id) {
    // 'no_data' cells should not be included in the link or path between habitats or patches
    //
    // if the cost_map's row'th and col'th element is not a no_data element then create the link,
    // otherwise ignore it.
    if (!std::isnan(cost_map[row][col]) && !std::isnan(cost_map[ac->row][ac->column])) {
      findPath(iLinkMap[(ac->row)][(ac->column)], iLinkMap[row][col], out_data->link_data);
    }
  }
}

//' Write error message
//'
//' @author Sam Doctolero
//' @keywords internal
void Engine::writeErrorMessage(char* msg, int size) {
  // if the msg's number of characters is greater than the allocated memory for error_message
  // then characters in msg cannot be inserted into error_message due to insufficient allocated space
  // therefore do not alter error_message
  if (size > error_message_size) {
    return;
  }

  // otherwise, insert all the characters of msg into error_message
  // once all of msg has been transferred then set the rest of error_message to null
  for (unsigned int i = 0; i < error_message_size; i++) {
    if (i < size) {
      error_message[i] = msg[i];
    } else {
      error_message[i] = 0;
    }
  }
}

//' Find the maximum value in a vector
//'
//' @author Sam Doctolero
//' @family C++ helper functions
//' @keywords internal
float Engine::emax(std::vector<float> vec) {
  // set the first element to the instantiated floating point variable called 'ret'
  float ret = vec[0];
  // parse through the rest of the vector
  for (unsigned int i = 1; i < vec.size(); i++) {
    // if ret's value is less than the i'th element of vec then set ret to that element
    if (ret < vec[i]) {
      ret = vec[i];
    }
  }
  return ret;
}

//' Find the minimum value in a vector
//'
//' @author Sam Doctolero
//' @family C++ helper functions
//' @keywords internal
float Engine::emin(std::vector<float> vec) {
  // set the first element to the instantiated floating point variable called 'ret'
  float ret = vec[0];
  // if ret's value is greater than the i'th element of vec then set ret to that element
  for (unsigned int i = 1; i < vec.size(); i++) {
    if (ret > vec[i]) {
      ret = vec[i];
    }
  }
  return ret;
}

//' Check whether a cell lies within the boundaries of a map
//'
//' Returns \code{false} if row or col is outside the map's boundaries.
//'
//' @author Sam Doctolero
//' @keywords internal
bool Engine::outOfBounds(int row, int col, int nrow, int ncol) {
  if (row < 0 || row >= nrow || col < 0 || col >= ncol) {
    return true;
  }
  return false;
}

//' Calculate Euclidean distance between two cells
//'
//' @note Assumes cells are square.
//'
//' @author Sam Doctolero
//' @keywords internal
float Engine::calcDistance(Cell c1, Cell c2) {
  int dr = c1.row - c2.row;
  int dc = c1.column - c2.column;
  return sqrt((float)(dr*dr) + (float)(dc*dc));
}

//' Check whether two cells have equal values
//'
//' Returns \code{true} if the rows and columns of c1 and c2 match.
//'
//' @author Sam Doctolero
//' @keywords internal
bool Engine::cellsEqual(Cell c1, Cell c2) {
  if (c1.row == c2.row && c1.column == c2.column) {
    return true;
  }
  return false;
}

//' Identify patches of cells within a map
//'
//' Finds clumps of adjacent cells with similar values and identifies them as patches.
//' Updates \code{voronoi_map}.
//'
//' @author Sam Doctolero
//' @family C++ patch functions
//' @keywords internal
void Engine::findPatches() {
  // declare and initiate an idCount and set it to 5
  int idCount = 5;

  // loop through all rows and columns of the input data
  for (int row = 0; row < in_data->nrow; row++) {
    for (int col = 0; col < in_data->ncol; col++) {
      // if the row'th and col'th element of the cost_map is equal to the habitat
      // then it is a patch or part of a patch
      if (in_data->patch_vec[row*in_data->ncol + col] > 0.0f) {
        // go through the adjacent cells and check if this is a new patch or just part of a patch
        int ind1 = -1;  // set an index to -1 called 'ind1'
        // top left
        if (!outOfBounds(row - 1, col - 1, in_data->nrow, in_data->ncol) &&
            in_data->patch_vec[(row - 1)*in_data->ncol + (col - 1)] > 0.0f) {
          // find the index of the patch from the patch list (ret) given the value
          // of the voronoi_map on the top left of the row'th and col'th element
          ind1 = getIndexFromList(voronoi_map[row - 1][col - 1], out_data->patch_list);
        }
        // left
        if (!outOfBounds(row, col - 1, in_data->nrow, in_data->ncol) &&
            in_data->patch_vec[row*in_data->ncol + (col - 1)] > 0.0f) {
          // find the index of the patch from the patch list (ret) given the value
          // of the voronoi_map on the left of the row'th and col'th element
          ind1 = getIndexFromList(voronoi_map[row][col - 1], out_data->patch_list);
        }

        // top and top right will be in the same patch
        int ind2 = -1;  // set the index to -1 called 'ind2'
        // top right
        if (!outOfBounds(row - 1, col + 1, in_data->nrow, in_data->ncol) &&
            in_data->patch_vec[(row - 1)*in_data->ncol + (col + 1)] > 0.0f) {
          // find the index of the patch from the patch list (ret) given the value
          // of the voronoi_map on the top right of the row'th and col'th element
          ind2 = getIndexFromList(voronoi_map[row - 1][col + 1], out_data->patch_list);
        }
        // top
        if (!outOfBounds(row - 1, col, in_data->nrow, in_data->ncol) &&
            in_data->patch_vec[(row - 1)*in_data->ncol + col] > 0.0f) {
          // find the index of the patch from the patch list (ret) given the value
          // of the voronoi_map on the top of the row'th and col'th element
          ind2 = getIndexFromList(voronoi_map[row - 1][col], out_data->patch_list);
        }

        // declare and initiate another index variable called 'finalInd'
        int finalInd = -1;

        // go through cases:
        if (ind1 == ind2) {
          // if index1 and index 2 are equal then the set finalInd to either one (I chose ind1)
          finalInd = ind1;
        } else if (ind1 == -1 && ind2 != -1) {
          // if ind1 == -1 and ind2 is some other number other than -1 then the cell
          // (row'th and col'th) belongs to the patch at the ind2 element of the patch list (ret)
          finalInd = ind2;
        } else if (ind1 != -1 && ind2 == -1) {
          // if ind1 is some other number other than -1 and ind2 is -1 then the cell
          // (row'th and col'th) belongs to the patch at the ind1 element of the patch list (ret)
          finalInd = ind1;
        } else if (ind1 != -1 && ind2 != -1) {
          // if they aren't -1 then cell (row'th and col'th) belongs to two different patches;
          // combine the two patches and return the index of the combined patch
          finalInd = combinePatches(ind1, ind2, out_data->patch_list);
        } else {
          finalInd = -1; // else set finaInd to -1, meaning a new patch must be created
        }

        // no index found then create a new patch
        if (finalInd == -1) {
          Patch temp;                            // create an instance of a Patch called 'temp'
          temp.id = (float)idCount++;            // give temp an id of idCount then increment idCount
          Cell c = { row, col, temp.id };        // set Cell c's properties to row, col, and temp's id
          voronoi_map[row][col] = temp.id;       // set the row'th and col'th element of the voronoi_map to temp's id
          temp.body.push_back(c);                // insert Cell c in temp's property called body (vector of Cells)
          out_data->patch_list.push_back(temp);  // insert temp in ret ( a vector of Patches)
        } else {
          // otherwise insert the cell (row'th and col'th) in the patch
          Cell c = { row, col, out_data->patch_list[finalInd].id };   // set Cell c's properties to row, col , and ret's id at the finalInd'th element
          out_data->patch_list[finalInd].body.push_back(c);           // insert Cell c inside the property body of ret at the finalInd'th element
          voronoi_map[row][col] = out_data->patch_list[finalInd].id;  // set the row'th and col'th element of the voronoi_map to ret's id at the finalInd'th element
        }
      }
    }
  }
}

//' Merge patches with similar values
//'
//' When encountering two patches with similar values, they are combined into a
//' single patch whose id matches that of the larger patch.
//'
//' @return integer corresponding to an index
//'
//' @author Sam Doctolero
//' @family C++ patch functions
//' @keywords internal
int Engine::combinePatches(int & ind1, int & ind2, std::vector<Patch> & list) {
  int ret = -1;              // create an instance of an indexing integer and set it to -1
  Patch * p1 = &list[ind1];  // create an instance of a pointer to a Patch called 'p1'
                             // and set it to list's element at index == ind1
  Patch * p2 = &list[ind2];  // create an instance of a pointer to a Patch called 'p2'
                             // and set it to list's element at index == ind2

  // if the Patch at p1 has a larger area or number of cells than the Patch at p2
  if (p1->body.size() > p2->body.size()) {
    // then transfer all of p2's cells to p1
    // set ret to ind1 - 1 if ind1 is greater than ind2
    // (because the list will decrease in size and this is to take that into account)
    if (ind1 > ind2) {
      ret = ind1 - 1;
    } else {
      ret = ind1;
    }

    // transfer all of p2's cells to p1 and update the patch map
    std::vector<Cell> cList = p2->body;

    float id = p1->id;
    for (unsigned int i = 0; i < cList.size(); i++) {
      p1->body.push_back(cList[i]);
      voronoi_map[cList[i].row][cList[i].column] = id;
    }

    list.erase(list.begin() + ind2); // remove the element at ind2 of list
  } else {
    // else if the Patch at p2 has a larger are or number of cells than the Patch at p1
    // then transfer all of p1's cells to p2

    // if ind2 is greater than ind1 then set ret to ind2 minus 1 due to the list's
    // number of elements decreasing later on
    if (ind2 > ind1) {
      ret = (ind2 - 1);
    } else {
      ret = ind2;
    }

    // transfer all of p1's cells to p2 and update the patch map
    std::vector<Cell> cList = p1->body;
    float id = p2->id;
    for (unsigned int i = 0; i < cList.size(); i++) {
      p2->body.push_back(cList[i]);
      voronoi_map[cList[i].row][cList[i].column] = id;
    }
    // remove the element at ind1 of list
    list.erase(list.begin() + ind1);
  }
  // return the index of the combined patch
  return ret;
}

//' Get the index for a patch with a given id value
//'
//' @author Sam Doctolero
//' @family C++ patch functions
//' @keywords internal
int Engine::getIndexFromList(float & id, std::vector<Patch> & patches) {
  // go through all the patches in memory and check if the parameter 'id' is equal
  // to any of the patches; if not, then return -1 to indicate the absence of that patch.
  for (unsigned int i = 0; i < patches.size(); i++) {
    if (patches[i].id == id) {
      return i;
    }
  }
  return -1;
}

//' Create a new link connecting two cells in a map
//'
//' Given \code{ac} (a pointer to an \code{ActiveCell}) as the parent cell and the integer
//' parameters \code{row} and \code{col} as the child cell, and \code{cost} as the
//' child cell's resistance value, create a connection between the parent the child cells.
//'
//' @author Sam Doctolero
//' @family C++ linking functions
//' @keywords internal
void Engine::connectCell(ActiveCell * ac, int row, int col, float cost) {
  LinkCell lc;                      // create an instance of a LinkCell called 'lc'
  lc.row = row;                     // set lc's row to the paremeter row
  lc.column = col;                  // set lc's column to the parameter col
  lc.id = ac->id;                   // set lc's id to ac's id
  lc.cost = cost;                   // set lc's cost to the parameter cost
  lc.distance = ac->distance;       // set lc's distance to ac's distance
  lc.fromCell.row = ac->row;        // set lc's fromCell's row to ac's row
  lc.fromCell.column = ac->column;  // set lc's fromCell's column to ac's column
  lc.fromCell.id = lc.id;           // set lc's id to ac's id
  lc.originCell = ac->originCell;   // set lc's origin cell to ac's originCell

  iLinkMap[row][col] = lc;          // set the row'th an col'th element of iLinkMap to lc
}

//' Finds the least cost path between two patches
//'
//' Checks whether a path between patches exists in the \code{path_list}
//'
//' @param ac1,ac2 objects of \code{LinkCell}
//'
//' @param path_list vector of \code{Link}s
//'
//' @return No return value. As a side effect, updates \code{path_list} with
//'         new path linking \code{ac2} and \code{ac2}.
//'
//' @author Sam Doctolero
//' @family C++ linking functions
//' @keywords internal
void Engine::findPath(LinkCell &ac1, LinkCell &ac2, std::vector<Link> & path_list) {
  for (unsigned int i = 0; i < path_list.size(); i++) {
    // if the end id and start id correspond to ac1's id and ac2's id OR if the
    // start id and end id correspond to ac1's id and ac2's id then the link already exists
    if ((path_list[i].end.id == ac1.id && path_list[i].start.id == ac2.id) ||
        (path_list[i].start.id == ac1.id && path_list[i].end.id == ac2.id)) {
      return;
    }
  }

  // create the path
  Link path;         // create a Link instance and name it path
  path.cost = 0.0f;  // set the initial cost counter to zero

  LinkCell lc_temp; // instantiate 'lc_temp' (values set below)

  // start cell
  // get the LinkCell from iLinkMap using ac1's location (row and column)
  lc_temp = iLinkMap[ac1.row][ac1.column];
  // from ac1's location (or lc_temp's location) follow its connections until it reaches a patch
  path.start = parseMap(lc_temp, path); // will also update path.connection to lc_temp's value

  // end cell
  // get the LinkCell from iLinkMap using ac2's location (row and column)
  lc_temp = iLinkMap[ac2.row][ac2.column];
  // from ac2's location (or lc_temp's location) follow its connections until it reaches a path
  path.end = parseMap(lc_temp, path); // will also update path.connection to lc_temp's value

  // if no cheaper indirect path found, add path to path_list
  if (lookForIndirectPath(path_list, path)) {
    path_list.push_back(path);
  }
}

//' Parse a map
//'
//' Given a starting \code{LinkCell} it follows the connections until it reaches a patch.
//' The last cell in the connection is returned.
//'
//' @param lc object of type \code{LinkCell}
//'
//' @param path object of type \code{Link}.
//'
//' @return object of type \code{Cell} corresponding to the last cell in \code{path.connection}.
//'         As a side effect, updates the \code{cost} and \code{connection} fields of \code{path}.
//'
//' @author Sam Doctolero
//' @family C++ linking functions
//' @keywords internal
Cell Engine::parseMap(LinkCell lc, Link & path) {
  // create an instance of Cell c1, and  set the properties as lc's row, column, and id
  Cell c1 = { lc.row, lc.column, lc.id };

  // create an instance of Cell called ret (this will be returned)
  Cell ret = c1;

  // go through all the connections starting from the input parameter lc's location
  // in the iLinkMap property of the Engine object,
  // as long as c1 does not equal lc's originCell (a cell as part of a patch)
  while (!cellsEqual(c1, lc.fromCell)) {
    path.cost += lc.cost;                  // increment the cost by lc's cost
    path.connection.push_back(c1);         // insert the Cell c1 to path.connection

    ret = lc;                              // update ret to be lc

    Cell from = lc.fromCell;               // create an instance of a Cell called 'from'
                                           // and set it to lc's fromCell (which is
                                           // the parentCell or the next cell to process)

    lc = iLinkMap[from.row][from.column];  // set lc to the next LinkCell from the iLinkMap
    c1.column = lc.column;                 // set c1's paremeters to lc's new parameters
    c1.row = lc.row;
    c1.id = lc.id;
  }

  // return the final Cell that was processed (this is the start or end of the Link)
  return ret;
}

//' Find a 'cheaper' indirect path between two patches
//'
//' @param path_list vector of \code{Link}s
//'
//' @param path object of type \code{Link}
//'
//' @return logical indicating an indirect path was NOT found
//'         (i.e., \code{true} if not found; \code{false} if one is found).
//'         As a side effect, also updates \code{path.connection} if an indirect path is found.
//'
//' @author Sam Doctolero
//' @family C++ linking functions
//' @keywords internal
bool Engine::lookForIndirectPath(std::vector<Link> & path_list, Link & path) {
  // if path_list is empty then no indirect paths are available to search
  if (path_list.size() <= 0) {
    return true;
  }

  // otherwise parse through all the paths currently in the memory and check for lower cost ones
  for (unsigned int i = 0; i < path_list.size() - 1; i++) {
    // if the i'th path_list's start or end id equal to path's start id,
    // then this may be a possible indirect Link
    if (path_list[i].start.id == path.start.id || path_list[i].end.id == path.start.id) {
      // create an instance of a pointer to a Link called pPath1
      // and set it to the address of the i'th Link in the path_list
      Link * pPath1 = &path_list[i];

      // create a floating point variable called cost and initialize it with pPath1's cost
      float cost = pPath1->cost;

      // create a vector of Links called connection
      // (this will serve as the new indirect list of cells that create the Link)
      std::vector<Link> connection;

      // loop through all the links in path_list to compare costs
      for (unsigned int j = i + 1; j < path_list.size(); j++) {
        Rcpp::checkUserInterrupt();  // comment this when NOT using R

        // create an instance of a pointer to a Link called pPath2,
        // and set it to the j'th Link in the path_list
        Link * pPath2 = &path_list[j];

        // proceed if pPath1 and pPath2 do not point to the same address (same Link)
        if (pPath1 != pPath2) {
          cost += pPath2->cost;  // increment the cost variable with pPath2's cost

          // if the cost is not less than the parameter path's cost,
          // then the current indirect link is not valid
          // NOTE: in case of a tie, the first link (shorter distance) is used
          if (cost >= path.cost) {
            // decrement the cost variable with pPath2's cost and exit the inner loop
            cost -= pPath2->cost;
            break;
          }
          connection.push_back(*pPath1);  // otherwise, insert pPath1's Link in connection
          pPath1 = pPath2;

          // if the path's end patch is reached then this is the end of the processing,
          // and a proper indirect Link is found
          if (pPath2->end.id == path.end.id || pPath2->start.id == path.end.id) {
            path.cost = cost;
            path.connection = connection[0].connection;

            // parse through all the Links in connection to create a new Link,
            // then update path with that new Link
            for (unsigned int k = 1; k < connection.size(); k++) {
              for (unsigned int m = 0; m < connection[k].connection.size(); m++) {
                path.connection.push_back(connection[k].connection[m]);
              }
            }
            return false;
          }
        }
      }
    }
  }
  return true;
}
