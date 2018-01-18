#include<iostream>
#include<vector>
#include<string>
#include"mpi.h"
using namespace std;
typedef struct commInfo{
	int rank;
	int size;
	MPI_Comm comm;	
	string desc;
}commInfo;
class proc{
private:
	vector<commInfo> proc_comm;
public:
	proc();
	proc(MPI_Comm comm);
	bool add_in(MPI_Comm comm);
	void indentify();
};

proc::proc(MPI_Comm comm){
	commInfo commI;
	MPI_Comm_rank(comm,&commI.rank);
	MPI_Comm_size(comm,&commI.size);
	commI.desc="WORLD";
	proc_comm.push_back(commI);
}
bool proc::add_in(MPI_Comm comm){
	
}
int main(int argc, char* argv[]){
	MPI_Init(&argc, &argv);
	proc p(MPI_COMM_WORLD);
	p.indentify();
	MPI_Finalize();
	return 0;
}
