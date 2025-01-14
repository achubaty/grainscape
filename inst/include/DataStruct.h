#include <vector>

// base cell or pixel structure
struct Cell {
   // NOTE: initialized with these nonsense values,
   // but ensure they get properly initialized on first use
   int row = -99;
   int column = -99;
   float id = -99;
};

// stores all the links (directly and indirectly) between the patches
// Links are given a negative ID to distinguish them from patch IDs
struct Link {
   Cell start, end;               // start and end nodes
   std::vector<Cell> connection;  // collection of cells that create the link
   float cost = 0.0f;             // cost of the link or path
};

// a patch (cluster) of habitat pixels in the resistance map with similar values
struct Patch {
   std::vector<Cell> body;        // body of the patch
   float id;                      // id of the patch
};

// flMap is used to store the output data (voronoi, link, patches) and cost map,
// as a map/matrix container of floats
typedef std::vector<float> flCol;
typedef std::vector<flCol> flMap;

// LinkCell is used in a map to connect Cells together as the spreading progresses
struct LinkCell
   :Cell  // inherits from Cell structure
{
   // fromCell: Cell that it connects to
   // originCell: perimeter Cell in the patch that the link came from
   Cell fromCell, originCell;
   float distance = 0.0f;
   float cost = 0.0f;
};

// LinkMap is used to store the links among Cells as a map/matrix of LinkCells
typedef std::vector<LinkCell> lcCol;
typedef std::vector<lcCol> LinkMap;

// ActiveCell is used in the spreading algorithm
struct ActiveCell
   :Cell  // inherits from Cell structure
{
   float time, distance, resistance, parentResistance;
   Cell originCell;
};

// interface input data
struct InputData {
   std::vector<float> cost_vec;
   int nrow, ncol;
   std::vector<float> patch_vec;
};

// interface output data
struct OutputData {
   std::vector<float> voronoi_map, patch_map;
   std::vector<Link> link_data;
   std::vector<Patch> patch_list;
};

// active cell holder used for spreading
struct ActiveCellHolder {
   float value;
   std::vector<ActiveCell> list;

   // adds the ActiveCell c in ascending order by Euclidean distance
   void add(ActiveCell c) {
      if (list.size() <= 0) {
         list.push_back(c);
      } else {
         for (int i = list.size() - 1; i >= 0; i--) {
            if (list[i].distance <= c.distance) {
               list.insert(list.begin() + i + 1, c);
               break;
            }
         }
      }
   }

   // returns size of the vector
   unsigned int size() {
      return list.size();
   }
};

// queue for active cell holders used for spreading
struct ActiveCellQueue {
   std::vector<ActiveCellHolder> holder_list;

   // inserts the ActiveCellHolder h in ascending order by value (Euclidean distance)
   void insertH(ActiveCellHolder h) {
      int index = 0;
      bool found = false;

      if (holder_list.size() <= 0) {
         holder_list.push_back(h);
      } else {
         for (int i = holder_list.size() - 1; i >= 0; i--) {
            if (holder_list[i].value <= h.value) {
               if (holder_list[i].value < h.value) {
                  index = i + 1;
               } else {
                  index = i;
                  found = true;
               }
               break;
            }
         }

         if (found) {
            for (unsigned int i = 0; i < h.list.size(); i++) {
               holder_list[index].add(h.list[i]);
            }
         } else {
            holder_list.insert(holder_list.begin() + index, h);
         }
      }
   }

   // returns the size of the vector
   unsigned int size() {
      return holder_list.size();
   }
};
