#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <numeric>
#include <cmath>

using namespace std;

int nodefile (string name, vector<double> &node_num, vector<double> &xn,\
    vector<double> &yn, vector<double> &zn)
{
    //8:12 start + 18360s cout << "before node file" << endl;
    fstream fin;
    fin.open(name); 
    string line;
    string word;
    string temp;
    vector<string> result;

    string substr;

    // Open an existing file 
    while (!fin.eof( ))      //if not at end of file, continue reading numbers
    {
        result.clear(); //clear vector for every new line
        getline(fin, line); //get line from file
        //cout << "line: " << line << endl;
        stringstream s_stream(line);

        while(s_stream.good()) //sperate delim values while not at end of line
        {
            getline(s_stream,substr,','); //get first string delimited by comma
            result.push_back(substr);
        }

        //double n = std::stod(result.at(0));
        //cout << "n: " << n << endl;
        try 
        {
            node_num.push_back(stof(result.at(0)));
            xn.push_back(stof(result.at(1)));
            yn.push_back(stof(result.at(2)));
            zn.push_back(stof(result.at(3)));
        } 
        
        catch (const std::invalid_argument& e) 
        {

        } 
        //append values to end of vector

        //std::cout << std::setprecision(2) << std::fixed << v << std::endl;
        //node_num.push_back(n)
    }

    fin.close();

    return 0;
}

int elementfile (string name, vector<double> &el_num, vector<double> &one,\
    vector<double> &two, vector<double> &three, vector<double> &four,\
    vector<double> &five, vector<double> &six, vector<double> &seven,\
    vector<double> &eight)
{
    //cout << "before el file" << endl;
    fstream fin;
    fin.open(name); 
    string line;
    vector<string> result;
    string substr;

    // Open an existing file 
    while (!fin.eof( ))      //if not at end of file, continue reading numbers
    {
        result.clear(); //clear vector for every new line
        getline(fin, line); //get line from file
        //cout << "line: " << line << endl;
        stringstream s_stream(line);

        while(s_stream.good()) //sperate delim values while not at end of line
        {
            getline(s_stream,substr,','); //get first string delimited by comma
            result.push_back(substr);
        }

        //double n = std::stod(result.at(0));
        //cout << "n: " << n << endl;
        try 
        {
            el_num.push_back(stod(result.at(0)));
            one.push_back(stof(result.at(1)));
            two.push_back(stof(result.at(2)));
            three.push_back(stof(result.at(3)));
            four.push_back(stof(result.at(4)));
            five.push_back(stof(result.at(5)));
            six.push_back(stof(result.at(6)));
            seven.push_back(stof(result.at(7)));
            eight.push_back(stof(result.at(8)));
        } 
        
        catch (const std::invalid_argument& e) 
        {

        } 
        //append values to end of vector

        //std::cout << std::setprecision(2) << std::fixed << v << std::endl;
        //node_num.push_back(n)
    }

    fin.close();

    return 0;
}

int findNodes (int elnum, vector<double> &el_num, float elem_node[8][4],\
    vector<double> &one, vector<double> &two, vector<double> &three,\
    vector<double> &four, vector<double> &five, vector<double> &six,\
    vector<double> &seven, vector<double> &eight, vector<double> &xn,\
    vector<double> &yn, vector<double> &zn)
    
{
    //cout << "before find" << endl;
    int no_num;
    for (int k = 0; k<8;k++)
    {
        int nodes_in_el[8] = {one.at(elnum-1),two.at(elnum-1),three.at(elnum-1),\
                          four.at(elnum-1),five.at(elnum-1),six.at(elnum-1),\
                          seven.at(elnum-1),eight.at(elnum-1)};
        no_num = nodes_in_el[k];
        elem_node [k][0] = no_num;
        elem_node [k][1] = xn.at(no_num-1);
        elem_node [k][2] = yn.at(no_num-1);
        elem_node [k][3] = zn.at(no_num-1);

    }

    return 0;
}

int laserpath(string name, vector<double> &t, vector<double> &x,\
    vector<double> &y, vector<double> &z,vector<double> &p)
{
    //cout << "before las" << endl;
    fstream fin;
    fin.open(name); 
    string line;
    vector<string> result;
    string substr;

    // Open an existing file 
    while (!fin.eof( ))      //if not at end of file, continue reading numbers
    {
        result.clear(); //clear vector for every new line
        getline(fin, line); //get line from file
        //cout << "line: " << line << endl;
        stringstream s_stream(line);

        while(s_stream.good()) //sperate delim values while not at end of line
        {
            getline(s_stream,substr,','); //get first string delimited by comma
            result.push_back(substr);
        }

        //double n = std::stod(result.at(0));
        //cout << "n: " << result.at(0) << endl;

        //append values to end of vector
        try 
        {
            t.push_back(stof(result.at(0)));
            x.push_back(stof(result.at(1)));
            y.push_back(stof(result.at(2)));
            z.push_back(stof(result.at(3)));
            p.push_back(stof(result.at(4)));
        } 
        
        catch (const std::invalid_argument& e) 
        {

        } 


        //std::cout << std::setprecision(2) << std::fixed << v << std::endl;
        //node_num.push_back(n)
    }

    fin.close();
    return 0;
}

bool check_value(int num, vector<double> &elem_list)
{
    bool check1;
    
    for (int k = 0; k <elem_list.size() ;k++)
    {
        if (num == elem_list.at(k))
        {
            check1 = true;
            break;
        }
        else
        {
            //cout << "num: " << num << endl;
            check1 = false;
        }
    }
    return check1;
}


int main()
{

    vector<double> node_num;
    vector<double> xn, yn, zn;
    vector<double> x, y, z, t, p;
    vector<double> el_num;
    vector<double> elem_list;
    vector<double> elem_time;
    vector<double> one, two, three, four, five, six, seven, eight;
    float elem_node [8][4];


    //cout << "before" << endl;
    nodefile("/Users/Shaun/Desktop/PhD-September_2021/welding_model/nodes.csv", node_num, xn, yn, zn);

    elementfile("/Users/Shaun/Desktop/PhD-September_2021/welding_model/els.csv", el_num, one, two, three,\
                four, five, six, seven, eight);

    laserpath("/Users/Shaun/Desktop/PhD-September_2021/welding_model/gcode_line.csv", t,x,y,z,p);

    // ------------------- Element centroid -----------
    int el_start = 70001;
    int el_end = 195000;

    vector<double> elno, xi, yi, zi;
    vector<double> xi_new, yi_new, zi_new;

    for (int k = el_start; k< el_end+1; k++)
    {
        xi_new.clear();
        yi_new.clear();
        zi_new.clear();
        findNodes(k, el_num, elem_node, one, two, three,\
                  four, five, six, seven, eight,\
                  xn, yn, zn);

        for (int j=0;j<8;j++)
        {
            xi_new.push_back(elem_node[j][1]);
            yi_new.push_back(elem_node[j][2]);
            zi_new.push_back(elem_node[j][3]);
        }

        double sumx = accumulate(xi_new.begin(), xi_new.end(),\
                               decltype(xi_new)::value_type(0));
        double sumy = accumulate(yi_new.begin(), yi_new.end(),\
                               decltype(yi_new)::value_type(0));
        double sumz = accumulate(zi_new.begin(), zi_new.end(),\
                               decltype(zi_new)::value_type(0));
        //double sumz = accumulate(zi_new.begin(), zi_new.end(), 0);

        elno.push_back(k);
        xi.push_back(sumx/xi_new.size());
        yi.push_back(sumy/yi_new.size());
        zi.push_back(sumz/zi_new.size());

    }

    // --------------- Element Activation -----------------
    struct el_activ 
    { 
    vector <double> elem_list;
    vector <double> elem_time;
    };
    int count;
    for (int index = 0; index < t.size() ; index++)
    {
        int cnt = 0;
        double xa = x.at(index);
        double ya = y.at(index);
        double za = z.at(index);
        double ta = t.at(index);
        double pa = p.at(index);
        for (int el_count = 0; el_count < xi.size(); el_count++)
        {
            double xc = xi.at(el_count);
            double yc = yi.at(el_count);
            double zc = zi.at(el_count);
            int elnum = elno.at(el_count);
            double dist = sqrt(pow((xa-xc),2) + pow((ya-yc),2));
            if (pa == 0)
            {
                if (yc <= ya)
                {
                    //cout << "dist: " << dist << endl;
                    if (check_value(elnum,elem_list) == false)
                    {
                        //cout << "el: " << elno.at(el_count) << endl;

                        elem_list.push_back(elno.at(el_count));
                        elem_time.push_back(ta);
                        //cout << "el: " << elem_list.at(cnt) << " time: " \
                        //<<elem_time.at(cnt)<<endl;
                        cnt++;
                        
                        
                    }
                }
            }
        }
        count = index;
    }


    /*
    std::sort(people.begin(), people.end(), 
          [](auto const &a, auto const &b) { return a.age < b.age; });

    */

   
    string rtn = "/Users/Shaun/Desktop/PhD-September_2021/welding_model/elact.csv";
    //sort(.begin(), v.end());
    ofstream myfile(rtn);
    int vsize = elem_list.size();
    for (int n=0; n<vsize; n++)
    {   
        myfile << elem_list.at(n) << ", " << elem_time.at(n) << endl;
    }

    
    
    /*
    for (int n = 0; n<elem_list.size();n++)
    {
        cout << "el: " << elem_list.at(n) << " time: " <<elem_time.at(n)<<endl;

    }
    */
    //cout << elem_time.at(9) << endl;
    //cout << "el act size: " << elem_list.size() << endl;
    //cout << "counter: " << count << endl;
    //cout << "el cent size: " << elem_list.size() << endl;

}