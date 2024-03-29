#ifndef TIGER_REGALLOC_REGALLOC_H_
#define TIGER_REGALLOC_REGALLOC_H_

#include "tiger/codegen/assem.h"
#include "tiger/codegen/codegen.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/temp.h"
#include "tiger/liveness/liveness.h"
#include "tiger/regalloc/color.h"
#include "tiger/util/graph.h"

namespace ra {

class Result {
public:
  temp::Map *coloring_;
  assem::InstrList *il_;

  Result() : coloring_(nullptr), il_(nullptr) {}
  Result(temp::Map *coloring, assem::InstrList *il)
      : coloring_(coloring), il_(il) {}
  Result(const Result &result) = delete;
  Result(Result &&result) = delete;
  Result &operator=(const Result &result) = delete;
  Result &operator=(Result &&result) = delete;
  ~Result(){};
};

class RegAllocator {
  /* TODO: Put your lab6 code here */
private:
  std::unique_ptr<ra::Result> result_;
  frame::Frame *frame_;
  assem::InstrList *instrList_;

  live::IGraphPtr interf_graph;
  live::MoveList *moves;
  tab::Table<temp::Temp,live::INode >*tmp_node_map;

  //precolored machine register node
  live::INodeListPtr precolored;

  // low-degree non-move-related nodes
  live::INodeListPtr simplify_worklist;
  // low-degree move-related nodes
  live::INodeList *freeze_worklist;
  // high-degree nodes
  live::INodeListPtr spill_worklist;
  // nodes marked for spilling
  live::INodeListPtr spilled_nodes;
  // temporary registers, not precolored and not yet processed
  live::INodeListPtr initial;
  // Registers been coalesced
  live::INodeListPtr coalesced_nodes;
  // Containing temporaries removed from the graph
  live::INodeListPtr select_stack;
  // nodes colored
  live::INodeListPtr colored_nodes;
  // regs should not be spilled
  live::INodeListPtr no_spill_temps;

  /* Move sets */

  // moves enabled for coalescing
  live::MoveList *worklist_moves;
  // moves not yet ready for coalescing
  live::MoveList *active_moves;
  // moves has been coalesced
  live::MoveList *coalesced_moves;
  // moves whose source and target interfere
  live::MoveList *constrained_moves;
  // moves that will no longer be considered for coalescing
  live::MoveList *frozen_moves;
  /* Other data structures */

  // A mapping from a node to the list of moves it is associated with
  tab::Table<live::INode, live::MoveList> *move_list;
  // an array containing the current degree of each node
  tab::Table<live::INode, int> *degree;
  // When a move (u, v) has been coalesced, v put in coalescedNodes and u put
  // back on some work-list, then alias[v]=u
  tab::Table<live::INode, live::INode> *alias;

  void LivenessAnalysis();

  void AddEdge(live::INodePtr u, live::INodePtr v);

  void Build();
  void MakeWorklist();
  void Simplify();
  void Coalesce();

  live::INodeListPtr Adjacent(live::INodePtr n);
  live::MoveList *NodeMoves(live::INodePtr n);
  bool MoveRelated(live::INodePtr n);

  void DecrementDegree(live::INodePtr m);
  void EnableMoves(live::INodeListPtr nodes);

  void AddWorkList(live::INodePtr u);
  bool Conservative(live::INodeListPtr nodes);
  bool OK(live::INodePtr t, live::INodePtr r);
  live::INodePtr GetAlias(live::INodePtr n);
  void Combine(live::INodePtr u, live::INodePtr v);
  void Freeze();
  void FreezeMoves(live::INodePtr u);
  void SelectSpill();
  col::Result AssignColors();
  void RewriteProgram();

  assem::InstrList *RemoveRedundantMove(temp::Map *coloring);
  bool IsRedundant(assem::Instr *instr, temp::Map *coloring);
  void ClearAndInit();
  int K;
public:
  RegAllocator(frame::Frame *frame, std::unique_ptr<cg::AssemInstr> assem_instr);
  void RegAlloc();
  std::unique_ptr<ra::Result> TransferResult() { return std::move(result_); }

};

} // namespace ra

#endif