#include<iostream>
#include<random>
#include<chrono>
#include<algorithm>
using namespace std;

typedef long long LL;
mt19937_64 rng(chrono::steady_clock::now().time_since_epoch().count()); //For LL

LL getRange(LL a, LL b){
   LL ran = b-a+1;
   return (rng()%ran)+a;
}

int main() {
  const double LOW = 0;
  const double HIGH = 0.75;
  cout << "Insert number of variables: ";
  long long n; cin >> n;
  long long varn = n;
  n = (1LL << n);
  int low = max(1LL, (LL)(LOW * n));
  int high = min(n, (LL)(HIGH * n));
  long long numberOfTerms = getRange(low, high);
  cout << "random number of terms: " << numberOfTerms << endl;
  vector <long long> allTerms(n);
  iota(begin(allTerms), end(allTerms), 0);
  shuffle(begin(allTerms), end(allTerms), rng);
  while((int)(allTerms.size()) > numberOfTerms) allTerms.pop_back();
  for(int i = 0;i < numberOfTerms;i++){
    cout << allTerms[i] << ",\n"[i == numberOfTerms - 1];
  }
  cout << "minimize_clauseset([";
  for(int i = 0;i < numberOfTerms;i++) {
    LL curterm = allTerms[i];
    cout << "[";
    for(int j = 0;j < varn;j++) {
      char cur = (char) (j + 'a');
      if((curterm>>j)&i) cout << "";
      else cout << "~";
      cout << cur;
      cout << ",]"[j == varn - 1];
    }
    cout << ",]"[i == numberOfTerms - 1];
  }
  cout << ", M)." << endl;
}