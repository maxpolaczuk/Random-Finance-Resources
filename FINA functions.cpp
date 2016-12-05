// ConsoleApplication1.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#define _USE_MATH_DEFINES
#include <cmath>
#include <iostream>

using namespace std;

double normalCFD(double value)
{
	return 0.5 * erfc(-value * M_SQRT1_2);
}

double CAPM(float beta, float mrp, float rf)
{
	// takes the stock's beta, MRP and RF rate to calc the stocks return...
	return rf + beta * mrp;

}

double BS(double rf, double stock_price, double strike, double Tt, double sdev)
{
	// now run the calculation:
	double d1;
	double d2;
	d1 = (1 / (sdev * pow(Tt, 0.5))) * (log(stock_price / strike) + (rf + pow(sdev, 2) / 2)*(Tt));
	d2 = d1 - sdev*pow(Tt, 0.5);

	// get the option price:
	return normalCFD(d1) * stock_price - normalCFD(d2) * strike * exp(-rf*Tt);
}

int main()
{

	cout << "Max's Finance Functions in C++\n\n> ";


	// run a while loop to go back to here: (if incorrect choice)
	int stop;
	stop = 0;

	while (stop == 0) {
		// choose whether CAPM or option pricing model
		cout << "Select Function: \n1.) CAPM\n2.) BS Call Option Pricing Model\n> ";
		int choice;
		cin >> choice;

		if (choice == 1) {
			// then CAPM:
			// assign the variables
			float beta;
			float mrp;
			float rf;
			cout << "Welcome to the CAPM Model...\n\n";
			//beta
			cout << "What is the stocks beta?\n";
			cin >> beta;

			// market risk premium
			cout << "\nWhat is the market risk premium?\n";
			cin >> mrp;

			// risk free rate
			cout << "\nWhat is the risk free rate?\n";
			cin >> rf;

			// return the calculation
			double cap_m;

			cap_m = CAPM(beta, mrp, rf);

			cout << "\nThe expected return of the stock is " << cap_m;

			// end the loop
			stop = 1;
		}

		else {

			if (choice == 2) {

				// then BS Model:
				cout << "Max's Euro Call Option Calculator in C++:\n\n> ";

				// Initialize risk free rate:
				double rf;
				rf = 0.4;

				// start with stock price today
				cout << "What is the current stock price?:\n> ";
				double stock_price; // initialize the stock price
				cin >> stock_price;

				// Next is strike price:
				cout << "\nWhat is the strike price (K)?:\n> ";
				double strike; // initialize the strike price (K)
				cin >> strike;

				// Next is time to maturity:
				cout << "\nIn how many days will the contract expire?:\n> ";
				double Tt; // days till maturity...
				cin >> Tt;

				// Stock price STD dev:
				cout << "\nWhat is the std-deviation of the stock (Risk)?:\n> ";
				double sdev; // days till maturity...
				cin >> sdev;
				// Return the option price...
				double opt_price;
				opt_price = BS(rf, stock_price, strike, Tt, sdev);
				cout << "\n\nThe current price of this option should be $" << opt_price << "!";

				// end the loop...
				stop = 1;
			}

			else {
				// nothing happens:
				cout << "Error! you didn't select a valid choice\nPlease try again.\n";
				// go back to where you were in the loop...
			}
		}
	}
	// This is where to end the program
	int endd;
	cin >> endd;

	return 0;
}
