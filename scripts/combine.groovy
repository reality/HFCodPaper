import groovy.json.JsonSlurper

// TODO add gs

def data = []
def textRecord = []
def js = new JsonSlurper()
new File('./data/manual_annotations').eachFile { f ->
  if(f.getName() =~ /json$/) {
    if(!textRecord.contains(f.text)) { // the horror. what the fuck?
      data << js.parseText(f.text)
      textRecord << f.text
    }
  }
}

def addRec = js.parseText(new File('data/additional/results (2).json').text)

def groups = []

def results = [:]
data.each { d ->
  if(!groups.any { it.contains(d.results.keySet()[0]) }) {
    groups << d.results.keySet() 
  }
  d.results.each { k, v ->
    if(!results.containsKey(k)) { results[k] = [] }  
    def a= v.answers['qq0.0']
    if(a != 'yes' && a != 'no') { a = 'no' }
    results[k] << a
  }
}

println groups
def out = ["doi\tgroup\ta1\ta2\ta3"]
results.each { k, v ->
def group = groups.findIndexOf {
  k in it
}
  if(v.size() != 2) {
    println "No two results for $k"
  }
  def additional = ''
  if(addRec.results.containsKey(k)) {
    additional = addRec.results[k].answers['qq0.0']
  }
  if(v[0] != v[1]) {
    println "disagreement for $k: ${v.join(' ')} $additional"
  }
  out += "$k\t$group\t${v.join('\t')}\t$additional"
}

new File('combined.tsv').text = out.join('\n')
