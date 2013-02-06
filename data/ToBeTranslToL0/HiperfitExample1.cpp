// Hiperfit example1.cpp : Defines the entry point for the console application.
//

//#include "stdafx.h"
#include <vector>
#include <cmath>
#include <stdio.h>

#include <time.h>
#include <sys/timeb.h>

// TIMING

typedef struct timeb mlfi_timeb;
#define mlfi_ftime ftime

#define mlfi_diff_time(t1,t2) \
  (t1.time - t2.time) * 1000 + (t1.millitm - t2.millitm)

using namespace std;

const unsigned int OUTER_LOOP_COUNT = 100;
const unsigned int NUM_X            = 256;
const unsigned int NUM_Y            = 32;
const unsigned int NUM_T            = 64;

//	grid
vector<double>			myX, myY, myTimeline;
unsigned				myXindex, myYindex;

//	variable
vector<vector<double> > myResult;

//	coeffs
vector<vector<double> > myMuX, myVarX, myMuY, myVarY;

//	operators
vector<vector<double> >	myDx, myDxx, myDy, myDyy;


void updateParams(const unsigned g, const double alpha, const double beta, const double nu)
{
	for(unsigned i=0;i<myX.size();++i)
		for(unsigned j=0;j<myY.size();++j)
		{
			myMuX[i][j] = 0.0;
			myVarX[i][j] = exp(2*(beta*log(myX[i]) + myY[j] - 0.5*nu*nu*myTimeline[g]));
			myMuY[i][j] = 0.0;
			myVarY[i][j] = nu*nu;
		}
}


void initGrid(const double s0, const double alpha, const double nu,const double t, const unsigned numX, const unsigned numY, const unsigned numT)
{
	myX.resize(numX);
	myY.resize(numY);
	myTimeline.resize(numT);

	for(unsigned i=0;i<numT;++i)
		myTimeline[i] = t*i/(numT-1);

	const double stdX = 20*alpha*s0*sqrt(t);
	const double dx = stdX/numX;
	myXindex = static_cast<unsigned>(s0/dx);

	for(unsigned i=0;i<numX;++i)
		myX[i] = i*dx - myXindex*dx + s0;

	const double stdY = 10*nu*sqrt(t);
	const double dy = stdY/numY;
	const double logAlpha = log(alpha);
	myYindex = numY/2;

	for(unsigned i=0;i<numY;++i)
		myY[i] = i*dy - myYindex*dy + logAlpha;


	myMuX.resize(numX);
	myVarX.resize(numX);
	myMuY.resize(numX);
	myVarY.resize(numX);
	for(unsigned i=0;i<numX;++i)
	{
		myMuX[i].resize(numY);
		myVarX[i].resize(numY);
		myMuY[i].resize(numY);
		myVarY[i].resize(numY);
	}
}

void initOperator(const vector<double>& x, vector<vector<double> >& Dx, vector<vector<double> >& Dxx)
{
	const unsigned n = x.size();

	Dx.resize(n);
	Dxx.resize(n);

	for(unsigned i=0;i<n;++i)
	{
		Dx[i].resize(3);
		Dxx[i].resize(3);
	}

	double dxl, dxu;

	//	lower boundary
	dxl		 =  0.0;
	dxu		 =  x[1] - x[0];

	Dx[0][0]  =  0.0;
	Dx[0][1]  = -1.0/dxu;
	Dx[0][2]  =  1.0/dxu;
	
	Dxx[0][0] =  0.0;
	Dxx[0][1] =  0.0;
	Dxx[0][2] =  0.0;
	
	//	standard case
	for(unsigned i=1;i<n-1;i++)
	{
		dxl      = x[i]   - x[i-1];
		dxu      = x[i+1] - x[i];

		Dx[i][0]  = -dxu/dxl/(dxl+dxu);
		Dx[i][1]  = (dxu/dxl - dxl/dxu)/(dxl+dxu);
		Dx[i][2]  =  dxl/dxu/(dxl+dxu);

		Dxx[i][0] =  2.0/dxl/(dxl+dxu);
		Dxx[i][1] = -2.0*(1.0/dxl + 1.0/dxu)/(dxl+dxu);
		Dxx[i][2] =  2.0/dxu/(dxl+dxu); 
	}

	//	upper boundary
	dxl		   =  x[n-1] - x[n-2];
	dxu		   =  0.0;

	Dx[n-1][0]  = -1.0/dxl;
	Dx[n-1][1]  =  1.0/dxl;
	Dx[n-1][2]  =  0.0;

	Dxx[n-1][0] = 0.0;
	Dxx[n-1][1] = 0.0;
	Dxx[n-1][2] = 0.0;
}


void setPayoff(const double strike)
{
	myResult.resize(myX.size());
	for(unsigned i=0;i<myX.size();++i)
	{
		myResult[i].resize(myY.size());
		double payoff = max(myX[i]-strike,0.0);
		for(unsigned j=0;j<myY.size();++j)
			myResult[i][j] = payoff;
	}
}



//	tridag
void tridag(
	const vector<double>&	a,
	const vector<double>&	b,
	const vector<double>&	c,
	const vector<double>&	r,
	int						n,
	vector<double>&			u,
	vector<double>&			uu)
{
	int j;
	double bet;

	bet  = 1.0/b[0];
	u[0] = r[0]*bet;
	for(j=1;j<n;j++)
	{
		uu[j] = c[j-1]*bet;
		bet   = 1.0/(b[j] - a[j]*uu[j]);
		u[j]  = (r[j] - a[j]*u[j-1])*bet;
	}
	for(j=n-2;j>=0;j--)
		u[j]  -= uu[j+1]*u[j+1];
}


void
rollback(const unsigned g)
{
	unsigned numX = myX.size(),
			 numY = myY.size();

	unsigned numZ = max(numX,numY);

	int k, l;
	unsigned i, j;

	int kl, ku, ll, lu;

	double dtInv = 1.0/(myTimeline[g+1]-myTimeline[g]);

	vector<vector<double> > u(numY,vector<double>(numX)), v(numX,vector<double>(numY));

	vector<double> a(numZ), b(numZ), c(numZ), y(numZ), yy(numZ);

	//	explicit x
	for(i=0;i<numX;i++)
	{
		kl =	 1*(i==0);
		ku = 2 - 1*(i==numX-1);
		for(j=0;j<numY;j++)
		{
			u[j][i] = dtInv*myResult[i][j];
			for(k=kl;k<=ku;k++)
				u[j][i] += 0.5*(myMuX[i][j]*myDx[i][k] + 0.5*myVarX[i][j]*myDxx[i][k])*myResult[i+k-1][j];
		}
	}

	//	explicit y
	for(j=0;j<numY;j++)
	{
		ll =	 1*(j==0);
		lu = 2 - 1*(j==numY-1);
		for(i=0;i<numX;i++)
		{
			v[i][j] = 0.0;
			for(l=ll;l<=lu;l++)
				v[i][j] +=     (myMuY[i][j]*myDy[j][l] + 0.5*myVarY[i][j]*myDyy[j][l])*myResult[i][j+l-1];

			u[j][i] += v[i][j]; 
		}
	}

	//	implicit x
	for(j=0;j<numY;j++)
	{
		for(i=0;i<numX;i++)
		{
			a[i] =		 - 0.5*(myMuX[i][j]*myDx[i][0] + 0.5*myVarX[i][j]*myDxx[i][0]);
			b[i] = dtInv - 0.5*(myMuX[i][j]*myDx[i][1] + 0.5*myVarX[i][j]*myDxx[i][1]);
			c[i] =		 - 0.5*(myMuX[i][j]*myDx[i][2] + 0.5*myVarX[i][j]*myDxx[i][2]);
		}

		tridag(a,b,c,u[j],numX,u[j],yy);
	}

	//	implicit y
	for(i=0;i<numX;i++)
	{
		for(j=0;j<numY;j++)
		{
			a[j] =		 - 0.5*(myMuY[i][j]*myDy[j][0] + 0.5*myVarY[i][j]*myDyy[j][0]);
			b[j] = dtInv - 0.5*(myMuY[i][j]*myDy[j][1] + 0.5*myVarY[i][j]*myDyy[j][1]);
			c[j] =		 - 0.5*(myMuY[i][j]*myDy[j][2] + 0.5*myVarY[i][j]*myDyy[j][2]);
		}

		for(j=0;j<numY;j++)
			y[j] = dtInv*u[j][i] - 0.5*v[i][j];

		tridag(a,b,c,y,numY,myResult[i],yy);
	}
}

double value(const double s0,const double strike, const double t, const double alpha, const double nu, const double beta)
{
	//unsigned numX = 200, numY = 20, numT = 50;
    const unsigned int numX = NUM_X, numY = NUM_Y, numT = NUM_T;
	
	initGrid(s0,alpha,nu,t, numX, numY, numT);
	initOperator(myX,myDx,myDxx);
	initOperator(myY,myDy,myDyy);

	setPayoff(strike);
	for(int i = myTimeline.size()-2;i>=0;--i)
	{
		updateParams(i,alpha,beta,nu);
		rollback(i);
	}

	return myResult[myXindex][myYindex];
}

int main()
{
	const double s0 = 0.03, strike = 0.03, t = 5.0, alpha = 0.2, nu = 0.6, beta = 0.5;
	
	vector<double> strikes(OUTER_LOOP_COUNT),res(OUTER_LOOP_COUNT);

	for(unsigned i=0;i<OUTER_LOOP_COUNT;++i)
		strikes[i] = 0.001*i;

    {    // start timer
        mlfi_timeb  t_start, t_end;
        unsigned long int elapsed;
        mlfi_ftime(&t_start);

        // the main computational kernel!
    	for(unsigned i=0;i<OUTER_LOOP_COUNT;++i) {
	    	res[i] = value(s0,strikes[i],t,alpha,nu,beta);
        }

        mlfi_ftime(&t_end);
        elapsed = mlfi_diff_time(t_end,t_start);
        printf("\n\nOriginal Nordea CPU Run Time: %lu !\n\n", elapsed);
    }

    for( int k=0; k<OUTER_LOOP_COUNT; ++k ) {
        printf("(res[%d]: %f)\n", k, res[k]);
    }


	return 0;
}
