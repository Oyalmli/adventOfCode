#include <chrono>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <numeric>
#include <vector>

using namespace std;

int main(int argc, char **argv)
{
    auto start = chrono::high_resolution_clock::now();
    ifstream input(argv[1]);
    string line;
    vector<int> nums;
    nums.push_back(0);
    int counter = 1;
    for (string line; getline(input, line);)
    {
        nums.push_back(stoi(line));
    }
    
    sort(nums.begin(), nums.end());
    nums.push_back(nums.back() + 3);

    adjacent_difference(nums.begin(), nums.end(), nums.begin());

    long total = 1;
    int count = -1;
    for (int n : nums)
    {
        if (n == 1)
        {
            count++;
        }
        else if (count > 0)
        {
            total *= 1 + (count * (count + 1) / 2);
            count = -1;
        }
        else
        {
            count = -1;
        }
    }
    cout << total << std::endl;

    auto stop = chrono::high_resolution_clock::now();
    auto duration = chrono::duration_cast<chrono::microseconds>(stop - start);
    cout << duration.count() << "μs" << endl;
}