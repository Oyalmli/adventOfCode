#include <chrono>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <numeric>

using namespace std;

int main(int argc, char** argv)
{
    ifstream input(argv[1]);

    auto start = chrono::high_resolution_clock::now();
    string line;
    int nums[110] = {0};
    int counter = 1;
    for (string line; getline(input, line);)
    {
        nums[counter++] = stoi(line);
    }

    int n = sizeof(nums) / sizeof(nums[0]);
    sort(nums, nums + n);

    nums[109] = nums[108] + 3;
    int between[110];
    adjacent_difference(nums, nums + 110, between);

    long total = 1;
    int count = -1;
    for (int i = 0; i < 110; i++)
    {
        if (between[i] == 1)
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