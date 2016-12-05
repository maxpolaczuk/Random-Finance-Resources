#include <iostream>
#include <string>
#include <cmath>

using namespace std;

double normalCFD(double value)
{
   return 0.5 * erfc(-value * M_SQRT1_2);
}

int main()
{

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
   
   // now run the calculation:
   double d1;
   double d2;
   double opt_price;
   
   d1 = ( 1 / (sdev * sqrt(Tt)) ) * (log(stock_price / strike) + (rf + pow(sdev,2)/2)*(Tt));
   d2 = d1 - sdev*sqrt(Tt);

   // get the option price!
   opt_price = normalCFD(d1)*stock_price - normalCFD(d2) * strike *exp(-rf*Tt);
   
   
   cout << "\n\nThe current price of this option should be $"<< opt_price << "!";
    

   
   return 0;
}

