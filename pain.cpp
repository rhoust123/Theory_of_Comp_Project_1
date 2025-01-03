#include <iostream>
#include <string>

using std::cout;
using std::endl;

int main()
{
  for (int h = 0; h < 9; h ++)
    {
      for (int w = 0; w < 9; w ++)
        {
          if (h != 4 && w != 4 && h != 2 && h != w)
            {
              cout << "(\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
              cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
              cout << "\\\\nNext: H\" h-remain ";
              cout << "\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
              cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
              cout << "\\\\nNext: W\")" << endl;
              if (h > 2 && (h - 3 != 4) && (h - 3 != w))
                {
                  cout << "(\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
                  cout << "\\\\nNext: H\"     h-up ";
                  cout << "\"H: (" << (h / 3) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
                  if (h - 3 == 2)
                    {
                      cout << "\\\\nFREEDOM\")" << endl;
                    }
                  else
                    {
                      cout << "\\\\nNext: W\")" << endl;
                    }
                }
              if ((h % 3) > 0 && (h - 1 != 4) && (h - 1 != w))
                {
                  cout << "(\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
                  cout << "\\\\nNext: H\"   h-left ";
                  cout << "\"H: (" << ((h / 3) + 1) << ", " << (h % 3) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
                  cout << "\\\\nNext: W\")" << endl;
                }
              if ((h % 3) < 2 && (h + 1 != 4) && (h + 1 != w))
                {
                  cout << "(\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
                  cout << "\\\\nNext: H\"  h-right ";
                  cout << "\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 2) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
                  if (h + 1 == 2)
                    {
                      cout << "\\\\nFREEDOM\")" << endl;
                    }
                  else
                    {
                      cout << "\\\\nNext: W\")" << endl;
                    }
                }
              if (h < 6 && (h + 3 != 4) && (h + 3 != w))
                {
                  cout << "(\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
                  cout << "\\\\nNext: H\"   h-down ";
                  cout << "\"H: (" << ((h / 3) + 2) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
                  cout << "\\\\nNext: W\")" << endl;
                }
              if (w > 2 && (w - 3 != 4))
                {
                  cout << "(\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
                  cout << "\\\\nNext: W\"     w-up ";
                  cout << "\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << (w / 3) << ", " << ((w % 3) + 1) << ")";
                  if (w - 3 == h)
                    {
                      cout << "\\\\nDEATH\")" << endl;
                    }
                  else
                    {
                      cout << "\\\\nNext: H\")" << endl;
                    }
                }
              if ((w % 3) > 0 && (w - 1 != 4))
                {
                  cout << "(\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
                  cout << "\\\\nNext: W\"   w-left ";
                  cout << "\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << (w % 3) << ")";
                  if (w - 1 == h)
                    {
                      cout << "\\\\nDEATH\")" << endl;
                    }
                  else
                    {
                      cout << "\\\\nNext: H\")" << endl;
                    }
                }
              if ((w % 3) < 2 && (w + 1 != 4))
                {
                  cout << "(\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
                  cout << "\\\\nNext: W\"  w-right ";
                  cout << "\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 2) << ")";
                  if (w + 1 == h)
                    {
                      cout << "\\\\nDEATH\")" << endl;
                    }
                  else
                    {
                      cout << "\\\\nNext: H\")" << endl;
                    }
                }
              if (w < 6 && (w + 3 != 4))
                {
                  cout << "(\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 1) << ", " << ((w % 3) + 1) << ")";
                  cout << "\\\\nNext: W\"   w-down ";
                  cout << "\"H: (" << ((h / 3) + 1) << ", " << ((h % 3) + 1) << ")";
                  cout << "\\\\nW: (" << ((w / 3) + 2) << ", " << ((w % 3) + 1) << ")";
                  if (w + 3 == h)
                    {
                      cout << "\\\\nDEATH\")" << endl;
                    }
                  else
                    {
                      cout << "\\\\nNext: H\")" << endl;
                    }
                }
            }
        }
    }
  return 0;
}
