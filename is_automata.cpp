#include <Rcpp.h>
#include <regex>
using namespace Rcpp;

class automata {
  
private:
  
  std::string url_fragment = "://";
  
  std::regex bots;
  std::regex generic_automata;
  std::regex specific_automata;
  
  bool is_automata_single(std::string user_agent){
    
    // If there's no UA, it's automata
    if(user_agent.size() == 0){
      return true;
    }
    
    // If there's a URL, it's automata
    if(user_agent.find(url_fragment) != std::string::npos){
      return true;
    }
    
    // Various flavours of automata, true
    if(regex_search(user_agent, bots) || regex_search(user_agent, generic_automata) || regex_search(user_agent, specific_automata)){
      return true;
    }
    
    return false;
  };
  
public:
  
  std::vector < bool > is_automata_vector(std::vector < std::string > user_agents){
    
    unsigned int input_size = user_agents.size();
    std::vector < bool > output(input_size);
    
    for(unsigned int i = 0; i < input_size; i++){
      output[i] = is_automata_single(user_agents[i]);
    }
    
    return output;
  };
  
  automata(){
    bots = std::regex("Pywikibot");
    generic_automata = std::regex("^(Faraday|HTTPC|Ruby|Java|\\.NET|PHP|Python|Apache|Scrapy|PycURL|libwww|Zend)", std::regex::icase);
    specific_automata = std::regex("^(Lagotto|Peggo|Recuweb|Magnus|MLD|find-link)");
  }
};

//[[Rcpp::export]]
std::vector < bool > is_automata(std::vector < std::string > user_agents){
  automata automata_inst;
  return automata_inst.is_automata_vector(user_agents);
}