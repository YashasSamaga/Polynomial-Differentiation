#include <iostream>
#include <fstream>
#include <string.h>
#include <ctype.h>
#include <sstream>
#include <math.h>

//Uncomment if you are using MSVC++ Compiler
//#pragma warning(disable: 4996) 

#define TOKEN_SIZE 64
#define MAX_EQUATION_SIZE 512

using namespace std;
/*****************************************************************************************************************************/
enum DIFFERENTIATOR_ERRORS
{
	NULL_EXPRESSION,
	UNIDENTIFIED_CHARACTER,
	SYNTAX_ERROR,
	DERIVATIVE_NOT_FOUND
};
enum DIFFERENTIATOR_TOKEN_TYPES
{
	NUMBER,
	VARIABLE,
	DELIMITER,
	EOE,
	UNKNOWN_CHARACTER
};
class differentiator 
{
	static const char *error_strings[];

	char * expression;

	void s_error(int err);
	void s_error(char * info);
	void s_error(int err, char * extra_info);	

	protected:
		ostringstream result;
		bool success;

		char token[TOKEN_SIZE], token_type;
		void get_token();

		void get_expression(char * exp) { strcpy(exp, result.str().c_str()); }
		void set_expression(char * exp) { expression = exp; }

	public:
		differentiator() : expression(nullptr)	{	}

		void find_derivative(char * exp);
		bool get_derivative(char * res);

		virtual bool get_derivative(char *res, double value, double &rval)
		{
			return get_derivative(res);
		}
};
const char *differentiator::error_strings[] =
{
	"Null/Empty Expression",
	"Unidentified Character",
	"Syntax Error",
	"Derivative hasn't been calculated"
};
void differentiator::s_error(int err)
{
	cout << "[ERROR] Differentiator (EID: " << err << ") >> " << error_strings[err] << endl;
}
void differentiator::s_error(char * info)
{
	cout << "[ERROR] Differentiator >> " << info << endl;
}
void differentiator::s_error(int err,char * extra_info)
{
	cout << "[ERROR] Differentiator (EID: " << err << ") >> " << error_strings[err] << endl << extra_info << endl;
}
void differentiator::get_token()
{
	register char * tmp = token;

	while (isspace(*expression)) expression++;
	if (isdigit(*expression))
	{
		token_type = DIFFERENTIATOR_TOKEN_TYPES::NUMBER;
		do
		{
			*tmp++ = *expression++;
		}
		while (isdigit(*expression) || *expression == '.');
	}
	else 
	{
		switch (*expression)
		{
			case 'x':	token_type = DIFFERENTIATOR_TOKEN_TYPES::VARIABLE; break;
			case '^':
			case '+':
			case '-':	token_type = DIFFERENTIATOR_TOKEN_TYPES::DELIMITER; break;
			case '\0':
			case '\r':	token_type = DIFFERENTIATOR_TOKEN_TYPES::EOE;	break;
			default:	token_type = DIFFERENTIATOR_TOKEN_TYPES::UNKNOWN_CHARACTER;	break;
		}
		*tmp++ = *expression++;
	}	
	*tmp = '\0';
}
void differentiator::find_derivative(char * exp)
{
	int state = 0; //0 expecting a coefficient,1 expecting a variable,2 expecting a ^,3 expecting power,4 expect end of sub-expression

	double coefficient, power;

	expression = exp;
	success = false;

	result.str("");
	result.clear();

	get_token();
	if (token_type == DIFFERENTIATOR_TOKEN_TYPES::EOE) { s_error(DIFFERENTIATOR_ERRORS::NULL_EXPRESSION); return; }

	do
	{				
		switch (token_type)
		{
			case DIFFERENTIATOR_TOKEN_TYPES::NUMBER:
			{
				switch (state)
				{
					case 0:	coefficient = atof(token); state = 1; break;
					case 3:	power = atof(token);	state = 4; break;
					default: s_error(DIFFERENTIATOR_ERRORS::SYNTAX_ERROR, "Invalid Expression");	return;
				}
				break;
			}
			case DIFFERENTIATOR_TOKEN_TYPES::VARIABLE:
			{
				switch (state)
				{
					case 0:	coefficient = 1.0;	state = 2;	break;
					case 1:	state = 2;	break;
					case 2:	s_error(DIFFERENTIATOR_ERRORS::SYNTAX_ERROR, "Unexpected variable encounter (Product Rule not supported)");	return;
					case 3: s_error(DIFFERENTIATOR_ERRORS::SYNTAX_ERROR, "Expected a number but found variable (Chain Rule not supported)"); return;
					case 4:	s_error(DIFFERENTIATOR_ERRORS::SYNTAX_ERROR, "Unexpected variable encounter");	return;
				}
				break;
			}
			case DIFFERENTIATOR_TOKEN_TYPES::DELIMITER:
			{
				switch (*token)
				{
					case '^':
					{
						if (state == 2)	state = 3;
						else
						{
							s_error(DIFFERENTIATOR_ERRORS::SYNTAX_ERROR,"Unexpected Power Symbol (Note that only variables can be raised to a power)");
							return;
						}
						break;
					}
					case '+':
					case '-':
					{ 
						if (state == 4)
						{
							if (power == 0)
							{
								state = 0;
								break;
							}

							coefficient *= power;
							power -= 1.0;

							if (power == 1.0)
							{
								if (coefficient == 1.0) result << "x " << *token<<' ';
								else result << coefficient << "x " << *token << ' ';
							}
							else if (power == 0) result <<coefficient << *token << ' ';
							else
							{
								if (coefficient == 1.0)	result << "x^" << power << ' ' << *token << ' ';
								else result << coefficient << "x^" << power << ' ' << *token << ' ';
							}
						}
						else if (state == 2) result << coefficient << ' ' << *token << ' ';
						else if (state != 1)
						{
							s_error(DIFFERENTIATOR_ERRORS::SYNTAX_ERROR);
							return;
						}
						state = 0;
						break;
					}
				}
				break;
			}
			case DIFFERENTIATOR_TOKEN_TYPES::EOE:
			{		
					switch (state)
					{
						case 0: s_error(DIFFERENTIATOR_ERRORS::SYNTAX_ERROR, "Trailing Operator"); return;
						case 1:
						{
							state = static_cast<int>(result.tellp());
							result.seekp(state - 3);
							result << '\0';
							break;
						}
						case 2:	result << coefficient;	break;
						case 3:	s_error(DIFFERENTIATOR_ERRORS::SYNTAX_ERROR, "Expected to a power after ^"); return;
						case 4:
						{
							if (power == 0)	{	state = 0;	break;	}

							coefficient *= power;
							power -= 1.0;

							if (power == 1.0)
							{
								if (coefficient == 1.0) result << "x " << *token << ' ';
								else result << coefficient << "x " << *token << ' ';
							}
							else if (power == 0) result << coefficient << *token << ' ';
							else
							{
								if (coefficient == 1.0)	result << "x^" << power << ' ' << *token << ' ';
								else result << coefficient << "x^" << power << ' ' << *token << ' ';
							}
							break; 
						}
					}
					success = true;
				return;
			}
			case DIFFERENTIATOR_TOKEN_TYPES::UNKNOWN_CHARACTER:	s_error(DIFFERENTIATOR_ERRORS::UNIDENTIFIED_CHARACTER,token);	return;			
		}
		get_token();
	} while (1);
}
bool differentiator::get_derivative(char * res)
{
	if (expression == nullptr)
	{
		s_error(DIFFERENTIATOR_ERRORS::DERIVATIVE_NOT_FOUND);
		return false;
	}
	if (success)	strcpy(res, result.str().c_str());	
	return success;
}
/*****************************************************************************************************************************/
class enhanced_differentiator : public differentiator
{
	double calculate(double value);
	public:
		bool get_derivative(char * res,double value,double &rval) override
		{
			if (success)
			{
				rval = calculate(value);
				get_expression(res);
			}
			return success;
		}
};
double enhanced_differentiator::calculate(double value)
{
	double rval=0,coeff,power=1;
	int state = 0;

	char diff_eqn[MAX_EQUATION_SIZE], previous_operation = 0;
	get_expression(diff_eqn);
	set_expression(diff_eqn);

	get_token();
	do
	{
		switch (token_type)
		{
			case DIFFERENTIATOR_TOKEN_TYPES::NUMBER:
			{				
				if (state == 0)
				{
					coeff = atof(token);
					power = 0;
				}
				else if (state == 2) power = atof(token);
				break;
			}
			case DIFFERENTIATOR_TOKEN_TYPES::VARIABLE:	state = 1; power = 1; break;
			case DIFFERENTIATOR_TOKEN_TYPES::DELIMITER:
			{
				switch (*token)
				{
					case '^':	state = 2;	break;
					case '+':
					case '-':
					{
						switch (previous_operation)
						{
							case '+':	rval += coeff*pow(value, power); break;
							case '-':	rval -= coeff*pow(value, power); break;
							case 0: rval = coeff*pow(value, power); break;
						}
						previous_operation = *token;	
						state = 0;
						break;
					}
				}
				break;
			}
			case DIFFERENTIATOR_TOKEN_TYPES::EOE:
			{
				switch (previous_operation)
				{
					case '+':	rval += coeff*pow(value, power); break;
					case '-':	rval -= coeff*pow(value, power); break;
					case 0: return coeff*pow(value,power); 
				}				
				return rval; 
			}
		}
		get_token();
	} while (1);
}
/*****************************************************************************************************************************/
int main()
{
	char expression[] = "5x^3 + 2x^2  + 6x + 4";
	char result[1000];
	double value;

	enhanced_differentiator diff_exp;
	diff_exp.find_derivative(expression);

	if(diff_exp.get_derivative(result,2,value))
	cout << value << " "<<result;

	cin.get();
	return 0;
}
